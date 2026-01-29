var order = 1;

function make_slides(f) {
    var slides = {};

    
    slides.i0 = slide({
        name : "i0",
        start: function() {
            exp.startT = Date.now();
        }
    });

    slides.instruction = slide({
        name : "instruction",
        button : function() {
            exp.startT = Date.now();
            exp.go(); //use exp.go() if and only if there is no "present" data.
        }
    });

    slides.reminder = slide({
        name : "reminder",
        button : function() {
          exp.go(); //use exp.go() if and only if there is no "present" data.
        }
        
      });

    slides.practice_1 = slide({
        name : "practice_1",
        /* trial information for this block
         (the variable 'stim' will change between each of these values,
          and for each of these, present_handle will be run.) */
        present : [{"a": 2}],
        start : function() {
            $(".forced_choice_err").hide(); // hide the error message   
        },
        // this gets run only at the beginning of the block
        present_handle : function(stim) {
          $(".forced_choice_err").hide();
          this.stim = stim;
          var target = "村民们出门修补____大风破坏了的房屋。"
          $(".target").html(target);

          // exp.first_response_wrong = 0;
          exp.response = undefined;
          exp.choice = undefined;
          exp.selected_content = undefined; 
          $('input[name="practice"]:checked').removeAttr("checked");
          var left_button = "被";
          $(".left_button").html(left_button);
          var right_button = "把";
          $(".right_button").html(right_button);
          // exp.incorrect_attempts = 0;
        },
        button : function() {
          exp.response = $('input[name="practice"]:checked').val()

          if (exp.response == undefined) {
            $(".forced_choice_err").show();
          } else {
            // we hard-coded that the left is the correct answer
            if (exp.response == "left") {
              exp.choice = "correct";
              exp.selected_content = "bei";
            } else {
              exp.choice = "incorrect";
              exp.selected_content = "ba";
            }
            this.log_responses();
            _stream.apply(this);
          }
          console.log(exp.choice);
        },
        log_responses : function() {
          // console.log("response: "+ exp.response)
          exp.data_trials.push({
            "trial_num" : 0,
            "item_id" : "202",
            "block_id" : "practice",
            "condition" : "practice_1",
            "verb": "NA",
            "response" : exp.choice,
            "original_choice" : exp.selected_content
          });
    
        }
      });

      slides.post_practice_1 = slide({
        name : "post_practice_1",
        button : function() {
          exp.go(); //use exp.go() if and only if there is no "present" data.
        }
      });

      slides.practice_2 = slide({
        name : "practice_2",
    
        /* trial information for this block
         (the variable 'stim' will change between each of these values,
          and for each of these, present_handle will be run.) */
        present : [{"a": 3}],
        start : function() {
            $(".forced_choice_err").hide(); // hide the error message   
        },
        // this gets run only at the beginning of the block
        present_handle : function(stim) {
            $(".forced_choice_err").hide();
            this.stim = stim;
            var target = "你____这个颜色涂在这里。"
            $(".target").html(target);
            // exp.first_response_wrong = 0;
            exp.response = undefined;
            exp.choice = undefined;
            exp.selected_content = undefined; 
            $('input[name="practice"]:checked').removeAttr("checked");
            var left_button = "把";
            $(".left_button").html(left_button);
            var right_button = "被";
            $(".right_button").html(right_button);
            // exp.incorrect_attempts = 0;
        },
        button : function() {
            exp.response = $('input[name="practice"]:checked').val();

            if (exp.response == undefined) {
                $(".forced_choice_err").show();
            } else {
              // we hard-coded that the left is the correct answer
              if (exp.response == "left") {
                exp.choice = "correct";
                exp.selected_content = "ba";
              } else {
                exp.choice = "incorrect";
                exp.selected_content = "bei";
              }
              this.log_responses();
              _stream.apply(this);
            }
            console.log(exp.choice);
        },
        log_responses : function() {
          // console.log("response: " + exp.response)
          exp.data_trials.push({
            "trial_num" : 0,
            "item_id" : "203",
            "block_id" : "practice",
            "condition" : "practice_2",
            "verb": "NA",
            "response" : exp.choice,
            "original_choice" : exp.selected_content
          });
        }
      });

      slides.post_practice_2 = slide({
        name : "post_practice_2",
        button : function() {
          exp.go(); //use exp.go() if and only if there is no "present" data.
        }
      });
    
      slides.last_reminder = slide({
        name : "last_reminder",
        button : function() {
          exp.go(); //use exp.go() if and only if there is no "present" data.
        }
        
      });


    slides.block1 = slide({
        name : "block1",
        present : exp.stims_block,
        start : function() {
            $(".forced_choice_err").hide();
        },
        
        present_handle : function(stim) {
            $('.bar').css('width', ( (100*(exp.phase)/exp.nQs) + "%"));    	    	    
            this.stim = stim;
            this.stim.trial_start = Date.now();      
            $(".forced_choice_err").hide();
            
            var target = this.stim.target;
            $(".target").text(target);
            $('input[name=critical]').hide();
            $(".left_button").hide();
            $(".right_button").hide();

            $('input[name="critical"]:checked').removeAttr("checked"); // remove the previous response
            exp.response = undefined; // remove the previous selection
            exp.choice = undefined; // remove the recorded choice
            exp.selected_content = undefined; // remove the recorded choice
            
            $(".left_button").show();
            $(".right_button").show();

            $('input[name=critical]').show();
            $('input[name="critical"]:checked').removeAttr("checked"); // remove response again
            exp.response = undefined;
            exp.choice = undefined;
            exp.selected_content = undefined;
  
            // the order of the buttons also need to be randomized
            var options = _.shuffle([this.stim.option_yiwei, this.stim.option_juede])
            // console.log("randomized order of choices: " + options)
            exp.left_button = options[0];
            $(".left_button").html(exp.left_button);
            exp.right_button = options[1];
            $(".right_button").html(exp.right_button);
            
            $(".continue_button").show(); // show the continue button
            $(".question").html(exp.question);

            console.log(this.stim); 
  
        },
        
        button : function() {

          exp.response = $('input[name="critical"]:checked').val();

          if (exp.response == undefined) {
              $(".forced_choice_err").show();
          } else {
            // do the additional step of getting the result of choice only 
            if (exp.response == "left") {
              var selected_content = exp.left_button;
            } else {
              var selected_content = exp.right_button;
            }
            exp.selected_content = selected_content;
            
            // record whether yiwei or juede is selected
            if (selected_content == this.stim.option_yiwei && this.stim.condition != "filler") {
              exp.choice = "yiwei";
            } else if (selected_content == this.stim.option_juede && this.stim.condition != "filler") {
              exp.choice = "juede";
            } else if (this.stim.condition == "filler") { // for filler items, the yiwei choice is the correct answer
              if (selected_content == this.stim.option_yiwei) {
                exp.choice = "correct";
              } else {
                exp.choice = "incorrect"
              }
            }
            console.log(exp.choice)
            this.log_responses();
            _stream.apply(this); //use exp.go() if and only if there is no "present" data.
          }
        },


        log_responses : function() {
            exp.data_trials.push({
                "trial_num" : order,
                "item_id" : this.stim.item,
                "block_id" : this.stim.block_id,
                "condition" : this.stim.condition,
                "verb": this.stim.verb,
                "response" : exp.choice,
                "original_choice" : exp.selected_content,
                "rt" : Date.now() - this.stim.trial_start
            });
            order += 1;
        }

    }); 
  
 
    slides.questionaire =  slide({
        name : "questionaire",
        submit : function(e){
        //if (e.preventDefault) e.preventDefault(); // I don't know what this means.
        exp.subj_data = {
            language : $("#language").val(),
            mandarin : $('input[name="ame"]:checked').val(),
            assess : $('input[name="assess"]:checked').val(),
            region : $("#region").val(),
            dialect : $("#dialect").val(),
            age : $("#age").val(),
            gender : $("#gender").val(),
            education : $("#education").val(),
            comments : $("#comments").val(),

        };
        exp.go(); //use exp.go() if and only if there is no "present" data.
        }
    });

    slides.finished = slide({
        name : "finished",
        start : function() {
        exp.data= {
            "trials" : exp.data_trials,
            "catch_trials" : exp.catch_trials,
            "system" : exp.system,
            "condition" : exp.condition,
            "subject_information" : exp.subj_data,
            "time_in_minutes" : (Date.now() - exp.startT)/60000
        };
        // record data using proliferate
        proliferate.submit(exp.data);
        }
    });
    console.log(slides);

    return slides;
}

function init() {
    
  // yiwei with original contrastive contexts (not p), a list (randomized)
    var yiwei = _.shuffle([
      {
        "item": "1",
        "verb": "yiwei",
        "condition": "yiwei_contrastive",
        "target": "妈妈____它是荷包蛋。",
        "option_yiwei": "以为",
        "option_juede": "觉得"
      },
      {
        "item": "2",
        "verb": "yiwei",
        "condition": "yiwei_contrastive",
        "target": "我____你不想坐了。",
        "option_yiwei": "以为",
        "option_juede": "觉得"
      },
      {
        "item": "3",
         "verb": "yiwei",
        "condition": "yiwei_contrastive",
        "target": "我____是要那个大车子。",
        "option_yiwei": "以为",
        "option_juede": "觉得"
      },
      {
        "item": "4",
        "verb": "yiwei",
        "condition": "yiwei_contrastive",
        "target": "我____他是小哥哥。",
        "option_yiwei": "以为",
        "option_juede": "觉得"
      },
      {
        "item": "5",
        "verb": "yiwei",
        "condition": "yiwei_contrastive",
        "target": "我____你是女生耶。",
        "option_yiwei": "以为",
        "option_juede": "觉得"
      },
      {
        "item": "6",
        "verb": "yiwei",
        "condition": "yiwei_contrastive",
        "target": "我____是婆婆做的。",
        "option_yiwei": "以为",
        "option_juede": "觉得"
      },
      {
        "item": "7",
        "verb": "yiwei",
        "condition": "yiwei_contrastive",
        "target": "我____是割破的。",
        "option_yiwei": "以为",
        "option_juede": "觉得"
      },
      {
        "item": "8",
        "verb": "yiwei",
        "condition": "yiwei_contrastive",
        "target": "我____没有橘黄色。",
        "option_yiwei": "以为",
        "option_juede": "觉得"
      },
      {
        "item": "9",
        "verb": "yiwei",
        "condition": "yiwei_contrastive",
        "target": "我还____你会睡你的新房间嘞！",
        "option_yiwei": "以为",
        "option_juede": "觉得"
      },
      {
        "item": "10",
        "verb": "yiwei",
        "condition": "yiwei_contrastive",
        "target": "哦，我____是人呢。",
        "option_yiwei": "以为",
        "option_juede": "觉得"
      }
    ]);
    
    // yiwei with original noncontrastive contexts (unclear), a list (randomized)
    var yiwei_unclear = _.shuffle([
      {
        "item": "101",
        "verb": "yiwei",
        "condition": "yiwei_unclear",
        "target": "不然爸爸____是我们弄坏掉的！",
        "option_yiwei": "以为",
        "option_juede": "觉得"
      },
      {
        "item": "102",
        "verb": "yiwei",
        "condition": "yiwei_unclear",
        "target": "他____那是一个大怪物。",
        "option_yiwei": "以为",
        "option_juede": "觉得"
      },
      {
        "item": "103",
        "verb": "yiwei",
        "condition": "yiwei_unclear",
        "target": "啊，我刚还____只有这一边呢。",
        "option_yiwei": "以为",
        "option_juede": "觉得"
      },
      {
        "item": "104",
        "verb": "yiwei",
        "condition": "yiwei_unclear",
        "target": "____小妹妹要抓小鸡哦！",
        "option_yiwei": "以为",
        "option_juede": "觉得"
      },
      {
        "item": "105",
        "verb": "yiwei",
        "condition": "yiwei_unclear",
        "target": "你消失了。我____只是一如往常的躲猫猫游戏。",
        "option_yiwei": "以为",
        "option_juede": "觉得"
      },
      {
        "item": "106",
        "verb": "yiwei",
        "condition": "yiwei_unclear",
        "target": "就____这两个巨人还在这里。",
        "option_yiwei": "以为",
        "option_juede": "觉得"
      },
      {
        "item": "107",
        "verb": "yiwei",
        "condition": "yiwei_unclear",
        "target": "出去人家都____你是小女生。",
        "option_yiwei": "以为",
        "option_juede": "觉得"
      },
      {
        "item": "108",
        "verb": "yiwei",
        "condition": "yiwei_unclear",
        "target": "它们都____它会欺负它啊。",
        "option_yiwei": "以为",
        "option_juede": "觉得"
      },
      {
        "item": "109",
        "verb": "yiwei",
        "condition": "yiwei_unclear",
        "target": "她____我走了。",
        "option_yiwei": "以为",
        "option_juede": "觉得"
      },
      {
        "item": "110",
        "verb": "yiwei",
        "condition": "yiwei_unclear",
        "target": "我____他知道要叫你把他弄。",
        "option_yiwei": "以为",
        "option_juede": "觉得"
      }
    ]);

    // juede with original contexts, a list (randomized)
    var juede = _.shuffle([
      {
        "item": "11",
        "verb": "juede",
        "condition": "juede_contrastive",
        "target": "____这顶比较可爱。",
        "option_yiwei": "以为",
        "option_juede": "觉得"
      },
      {
        "item": "12",
        "verb": "juede",
        "condition": "juede_contrastive",
        "target": "我____这个房子很漂亮。",
        "option_yiwei": "以为",
        "option_juede": "觉得"
      },
      {
        "item": "13",
        "verb": "juede",
        "condition": "juede_contrastive",
        "target": "哦，你____是洗澡用的淋蓬头。",
        "option_yiwei": "以为",
        "option_juede": "觉得"
      },
      {
        "item": "14",
        "verb": "juede",
        "condition": "juede_contrastive",
        "target": "我____是在这里。",
        "option_yiwei": "以为",
        "option_juede": "觉得"
      },
      {
        "item": "15",
        "verb": "juede",
        "condition": "juede_contrastive",
        "target": "哦，你____这是足球好圆呀。",
        "option_yiwei": "以为",
        "option_juede": "觉得"
      },
      {
        "item": "16",
        "verb": "juede",
        "condition": "juede_contrastive",
        "target": "我____盒子好像不是这一个。",
        "option_yiwei": "以为",
        "option_juede": "觉得"
      },
      {
        "item": "17",
        "verb": "juede",
        "condition": "juede_contrastive",
        "target": "你____是瓢虫喔。",
        "option_yiwei": "以为",
        "option_juede": "觉得"
      },
      {
        "item": "18",
        "verb": "juede",
        "condition": "juede_contrastive",
        "target": "他____地下室里也有大妖怪。",
        "option_yiwei": "以为",
        "option_juede": "觉得"
      },
      {
        "item": "19",
        "verb": "juede",
        "condition": "juede_contrastive",
        "target": "刚刚我看的也____很像。",
        "option_yiwei": "以为",
        "option_juede": "觉得"
      },
      {
        "item": "20",
        "verb": "juede",
        "condition": "juede_contrastive",
        "target": "你____是小狐狸的家，我们看看。",
        "option_yiwei": "以为",
        "option_juede": "觉得"
      }
    ]);

    // yiwei with original noncontrastive contexts (unclear), a list (randomized)
    var juede_unclear = _.shuffle([
      {
        "item": "111",
        "verb": "juede",
        "condition": "juede_unclear",
        "target": "我也____这个拼图有一点点难。",
        "option_yiwei": "以为",
        "option_juede": "觉得"
      },
      {
        "item": "112",
        "verb": "juede",
        "condition": "juede_unclear",
        "target": "我____你弄错了。",
        "option_yiwei": "以为",
        "option_juede": "觉得"
      },
      {
        "item": "113",
        "verb": "juede",
        "condition": "juede_unclear",
        "target": "我____她可能是一个邮递员阿姨。",
        "option_yiwei": "以为",
        "option_juede": "觉得"
      },
      {
        "item": "114",
        "verb": "juede",
        "condition": "juede_unclear",
        "target": "可是我____你的被风吹一下就倒了。",
        "option_yiwei": "以为",
        "option_juede": "觉得"
      },
      {
        "item": "115",
        "verb": "juede",
        "condition": "juede_unclear",
        "target": "我____它可以放在这里面。",
        "option_yiwei": "以为",
        "option_juede": "觉得"
      },
      {
        "item": "116",
        "verb": "juede",
        "condition": "juede_unclear",
        "target": "这种我____我也认不出来。",
        "option_yiwei": "以为",
        "option_juede": "觉得"
      },
      {
        "item": "117",
        "verb": "juede",
        "condition": "juede_unclear",
        "target": "你____在抽屉里啊！",
        "option_yiwei": "以为",
        "option_juede": "觉得"
      },
      {
        "item": "118",
        "verb": "juede",
        "condition": "juede_unclear",
        "target": "我____北极熊有可能追不上它。",
        "option_yiwei": "以为",
        "option_juede": "觉得"
      },
      {
        "item": "119",
        "verb": "juede",
        "condition": "juede_unclear",
        "target": "妈妈____这个猫咪它好幸福啊。",
        "option_yiwei": "以为",
        "option_juede": "觉得"
      },
      {
        "item": "120",
        "verb": "juede",
        "condition": "juede_unclear",
        "target": "他总是____怪兽在旁边，对吧？",
        "option_yiwei": "以为",
        "option_juede": "觉得"
      },
    ]);

    // fillers, a list (randomized)
    var filler = _.shuffle([
      {
        "item": "901",
        "verb": "filler",
        "condition": "filler",
        "target": "另外这个我们____没有看到过。",
        "option_yiwei": "以前",
        "option_juede": "以后"
      },
      {
        "item": "902",
        "verb": "filler",
        "condition": "filler",
        "target": "你不是____画过鱼吗？",
        "option_yiwei": "以前",
        "option_juede": "以后"
      },
      {
        "item": "903",
        "verb": "filler",
        "condition": "filler",
        "target": "玩过了____要收起来。",
        "option_yiwei": "以后",
        "option_juede": "以前"
      },
      {
        "item": "904",
        "verb": "filler",
        "condition": "filler",
        "target": "底下要搭稳了，____才能把上面搭高。",
        "option_yiwei": "以后",
        "option_juede": "以前"
      },
      {
        "item": "905",
        "verb": "filler",
        "condition": "filler",
        "target": "感觉像个世外____。",
        "option_yiwei": "桃源",
        "option_juede": "花园"
      },
      {
        "item": "906",
        "verb": "filler",
        "condition": "filler",
        "target": "确实，这样一摆真有点画龙____。",
        "option_yiwei": "点睛",
        "option_juede": "加眼"
      },
      {
        "item": "907",
        "verb": "filler",
        "condition": "filler",
        "target": "全是纸上____。",
        "option_yiwei": "谈兵",
        "option_juede": "说兵"
      },
      {
        "item": "908",
        "verb": "filler",
        "condition": "filler",
        "target": "但是我们不能过河____呀。",
        "option_yiwei": "拆桥",
        "option_juede": "拆船"
      },
      {
        "item": "909",
        "verb": "filler",
        "condition": "filler",
        "target": "加了反而有点____添足。",
        "option_yiwei": "画蛇",
        "option_juede": "画龙"
      },
      {
        "item": "910",
        "verb": "filler",
        "condition": "filler",
        "target": "妈妈:\t那我一鼓____先把药喝了。",
        "option_yiwei": "作气",
        "option_juede": "作力"
      }
    ]);

    num_blocks = 5
    num_per_block = 10
    total_blocks = []
    exp.stims_block = []
    for (var i=0; i<num_blocks; i++) {
        // each block will have 2 critical item (x4 condition) and 2 filler items
        // num_per_block == block.length == 10
        var block = [yiwei.pop(), yiwei_unclear.pop(),juede.pop(), juede_unclear.pop(), filler.pop(),
          yiwei.pop(), yiwei_unclear.pop(), juede.pop(), juede_unclear.pop(), filler.pop()
        ];
        // randomize the items within each block
        block = _.shuffle(block);
        console.log(block)
        total_blocks.push(block);
    }
    // randomize the order of blocks
	  total_blocks = _.shuffle(total_blocks); 
    console.log(total_blocks);

    // add block id (after shuffling) and add to exp.stims_block
    for (var b=0; b<num_blocks; b++) {
      var block =  total_blocks[b];
      for (var item=0; item<num_per_block; item++) {
        var stim = block[item];
        stim.block_id = b;
        exp.stims_block.push(jQuery.extend(true, {}, stim));
      }
    }

    console.log(exp.stims_block) 

    
    exp.trials = [];
    exp.catch_trials = [];
    // exp.condition = {}; // can randomize between subject conditions here -> not needed?
    exp.system = {
        Browser : BrowserDetect.browser,
        OS : BrowserDetect.OS,
        screenH: screen.height,
        screenUH: exp.height,
        screenW: screen.width,
        screenUW: exp.width
    };
    //blocks of the experiment:
    exp.structure=["i0", "instruction", "reminder",
    "practice_1", "post_practice_1",
    "practice_2", "post_practice_2",
    "last_reminder", "block1", 'questionaire', 'finished'];
    // console.log(exp.structure);

    exp.data_trials = [];
    //make corresponding slides:
    exp.slides = make_slides(exp);

    //   exp.nQs = utils.get_exp_length(); //this does not work if there are stacks of stims (but does work for an experiment with this structure)
                        //relies on structure and slides being defined
                        
    exp.nQs = 1 + 1 + 4 + 1 + 50; 
    $(".nQs").html(exp.nQs);

    $('.slide').hide(); //hide everything

    $("#start_button").click(function() {
        exp.go();
    });

    exp.go(); //show first slide
}
