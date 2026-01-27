var order = 1;

function make_slides(f) {
    var slides = {};

    
    slides.i0 = slide({
        name : "i0",
        start: function() {
            exp.startT = Date.now();
        }
    });

    slides.instructions = slide({
        name : "instructions",
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
            $(".err_good").hide();
        },
        // this gets run only at the beginning of the block
        present_handle : function(stim) {
          $(".forced_choice_err").hide();
          $(".err_good").hide();
          this.stim = stim;
          var context = "妈妈:\t妈妈给你讲个故事吧。\n孩子:\t好。\n妈妈:\t有一天夜里呢，刮了大风。\n妈妈:\t到天亮的时候，风才停下。\n妈妈:\t沼泽地的村民们纷纷从各自家里钻了出来。";
          var target = "妈妈:\t修补____大风破坏了的房屋。"
          var post_context = "妈妈:\t它们就修房子。\n妈妈\t蜘蛛阿尔丁昨天刚结的几个新网也破了几个大窟窿。";
          var full_sent = context + "\n" + target + "\n" + post_context
          $(".target").html(full_sent);

          // exp.first_response_wrong = 0;
          exp.response = undefined;
          exp.choice = undefined;
          exp.selected_content = undefined; 
          $('input[name="practice"]:checked').removeAttr("checked");
          var top_button = "把";
          $(".top_button").html(top_button);
          var bottom_button = "被";
          $(".bottom_button").html(bottom_button);
          // exp.incorrect_attempts = 0;
        },
        button : function() {
          exp.response = $('input[name="practice"]:checked').val()

          if (exp.response == undefined) {
            $(".forced_choice_err").show();
          } else {
            // we hard-coded that the bottom is the correct answer
            if (exp.response == "bottom") {
              exp.choice = "correct";
              exp.selected_content = "bei";
            } else {
              exp.choice = "incorrect";
              exp.selected_content = "ba";
            }
            this.log_responses();
            _stream.apply(this);
          }
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
            $(".err_good").hide();
        },
        // this gets run only at the beginning of the block
        present_handle : function(stim) {
            $(".forced_choice_err").hide();
            $(".err_good").hide();
            this.stim = stim;
            var context = "妈妈:\t你画太阳吧。\n孩子:\t你帮我画房子。\n妈妈:\t我帮你画房子啊。\n孩子:\t因为我不会画嘛。\n妈妈:\t不会画没关系。";
            var target = "妈妈:\t你____这个颜色涂在这里。"
            var post_context = "妈妈:\t这个是黑颜色。\n妈妈:\t这个电视机还少一样东西。";
            var full_sent = context + "\n" + target + "\n" + post_context
            $(".target").html(full_sent);
            // exp.first_response_wrong = 0;
            exp.response = undefined;
            exp.choice = undefined;
            exp.selected_content = undefined; 
            $('input[name="practice"]:checked').removeAttr("checked");
            var top_button = "把";
            $(".top_button").html(top_button);
            var bottom_button = "被";
            $(".bottom_button").html(bottom_button);
            // exp.incorrect_attempts = 0;
        },
        button : function() {
            exp.response = $('input[name="practice"]:checked').val();

            if (exp.response == undefined) {
                $(".forced_choice_err").show();
            } else {
              // we hard-coded that the top is the correct answer
              if (exp.response == "top") {
                exp.choice = "correct";
                exp.selected_content = "ba";
              } else {
                exp.choice = "incorrect";
                exp.selected_content = "bei";
              }
              this.log_responses();
              _stream.apply(this);
            }
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
            
            // var context = this.stim.context_full;
            var context = this.stim.original_context
            var target = this.stim.target;
            var post_context = this.stim.post_context;
            var full_sent = context + "\n" + target + "\n" + post_context
            $(".target").text(full_sent);
            $('input[name=critical]').hide();
            $(".top_button").hide();
            $(".bottom_button").hide();

            $('input[name="critical"]:checked').removeAttr("checked"); // remove the previous response
            exp.response = undefined; // remove the previous selection
            exp.choice = undefined; // remove the recorded choice
            exp.selected_content = undefined; // remove the recorded choice
            
            $(".top_button").show();
            $(".bottom_button").show();

            $('input[name=critical]').show();
            $('input[name="critical"]:checked').removeAttr("checked"); // remove response again
            exp.response = undefined;
            exp.choice = undefined;
            exp.selected_content = undefined;
  
            // the order of the buttons also need to be randomized
            var options = _.shuffle([this.stim.option_yiwei, this.stim.option_juede])
            // console.log("randomized order of choices: " + options)
            exp.top_button = options[0];
            $(".top_button").html(exp.top_button);
            exp.bottom_button = options[1];
            $(".bottom_button").html(exp.bottom_button);
            
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
            if (exp.response == "top") {
              var selected_content = exp.top_button;
            } else {
              var selected_content = exp.bottom_button;
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
        "original_context": "妈妈:\t好像不行哦。\n妈妈:\t是不是少了一片？\n妈妈:\t啊。(笑)\n妈妈:\t哦。\n妈妈:\t原来那个不是蛋啦。",
        "post_context": "妈妈:\t原来他是谁！\n妈妈:\t他是谁？",
        "target": "妈妈:\t妈妈____它是荷包蛋。",
        "option_yiwei": "以为",
        "option_juede": "觉得"
      },
      {
        "item": "2",
        "verb": "yiwei",
        "condition": "yiwei_contrastive",
        "original_context": "孩子:\t他看的是动画片。\n老师:\t动画片啊！\n老师:\t什么动画片啊？\n孩子:\t这是一个灯。\n老师:\t哦,你想坐下来啊",
        "post_context": "孩子:\t这是一个灯。\n老师:\t嗯,这是一个灯啊！",
        "target": "老师:\t我____你不想坐了。",
        "option_yiwei": "以为",
        "option_juede": "觉得"
      },
      {
        "item": "3",
         "verb": "yiwei",
        "condition": "yiwei_contrastive",
        "original_context": "实验员:\t哦。\n妈妈:\t哦。(笑)\n实验员:\t哦他在找这个啊。\n妈妈:\t嘿。\n实验员:\t原来是这小玩具车。",
        "post_context": "妈妈:\t奇怪哼。\n实验员:\t这小孩子喜欢。",
        "target": "实验员:\t我____是要那个大车子.",
        "option_yiwei": "以为",
        "option_juede": "觉得"
      },
      {
        "item": "4",
        "verb": "yiwei",
        "condition": "yiwei_contrastive",
        "original_context": "孩子:\t咦，他才不是小哥哥。\n妈妈:\t啊，那不是小哥哥。\n妈妈:\t我讲错了。\n妈妈:\t你看，你一直摸我这里。\n母亲\t我就忘记。",
        "post_context": "妈妈:\t所以你要专心啊。\n妈妈:\t不然我都会忘记啊。",
        "target": "妈妈:\t我____他是小哥哥.",
        "option_yiwei": "以为",
        "option_juede": "觉得"
      },
      {
        "item": "5",
        "verb": "yiwei",
        "condition": "yiwei_contrastive",
        "original_context": "孩子:\t等一下有...\n孩子:\tyeah.\n孩子:\t打开来。\n妈妈:\t你不是女生喔？\n孩子:\t我是弟弟。",
        "post_context": "妈妈:\t这样拿就好了。\n孩子:\t我要先拿绳子。",
        "target": "妈妈:\t我____你是女生耶。",
        "option_yiwei": "以为",
        "option_juede": "觉得"
      },
      {
        "item": "6",
        "verb": "yiwei",
        "condition": "yiwei_contrastive",
        "original_context": "大人:\t这个真的是做的很棒。\n大人:\t这个是谁做的？\n孩子:\t姨夫做的。\n大人:\t哦！\n大人:\t是姨夫做的啊？",
        "post_context": "大人:\t这么漂亮的一个汽车。\n大人:\t糟糕。",
        "target": "大人:\t我____是婆婆做的。",
        "option_yiwei": "以为",
        "option_juede": "觉得"
      },
      {
        "item": "7",
        "verb": "yiwei",
        "condition": "yiwei_contrastive",
        "original_context": "大人:\t你不是女生吗？\n大人:\t你这个手这么搞的？\n孩子:\t画画的。\n大人:\t哦，画画的不是割破的？\n孩子:\t嗯。",
        "post_context": "姐姐:\t啊，不要再乱打听。\n孩子:\t还有我耳朵好痛哦。",
        "target": "大人:\t我____是割破的。",
        "option_yiwei": "以为",
        "option_juede": "觉得"
      },
      {
        "item": "8",
        "verb": "yiwei",
        "condition": "yiwei_contrastive",
        "original_context": "妈妈:\t哎。\n妈妈:\t你要下去吃五香乖乖的话，你就存不下钱来。\n妈妈:\t你就没办法买很多很多的东西！\n孩子:\t什么啊？\n妈妈:\t这有橘黄色。",
        "post_context": "孩子:\t找橘黄色干嘛啊？\n妈妈:\t我要画颜色啊。",
        "target": "妈妈:\t我____没有橘黄色。",
        "option_yiwei": "以为",
        "option_juede": "觉得"
      },
      {
        "item": "9",
        "verb": "yiwei",
        "condition": "yiwei_contrastive",
        "original_context": "实验员:\t藏好！\n孩子:\t我要做了！\n实验员:\t你昨天睡这里吗？\n孩子:\t对啊。\n孩子:\t我昨天晚上就睡这啊！",
        "post_context": "孩子:\t我明天就可以睡了！\n实验员:\t等到收好就可以睡了嘛。",
        "target": "实验员:\t我还____你会睡你的新房间嘞！",
        "option_yiwei": "以为",
        "option_juede": "觉得"
      },
      {
        "item": "10",
        "verb": "yiwei",
        "condition": "yiwei_contrastive",
        "original_context": "实验员:\t这个。(手指着画)\n孩子:\t画的形状都很像哟。(手指着画)\n实验员:\t这是什么？(手指着画)\n孩子:\t这是鸟。\n实验员:\t这鸟。",
        "post_context": "实验员:\t啊。\n孩子:\t这我画的小鱼和水。",
        "target": "实验员:\t哦,我____是人呢。",
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
        "original_context": "实验员:\t放回去！\n实验员:\t你刚刚从这里面弄掉出来的。\n实验员:\t再把他放进去！\n孩子:\t不要！\n实验员:\t要。",
        "post_context": "孩子:\t爸爸哪有回来！\n实验员:\t等一下就回来了！",
        "target": "实验员:\t不然爸爸____是我们弄坏掉的！",
        "option_yiwei": "以为",
        "option_juede": "觉得"
      },
      {
        "item": "102",
        "verb": "yiwei",
        "condition": "yiwei_unclear",
        "original_context": "妈妈:\t这是一个好玩...好玩的东西。\n妈妈:\t你告诉蒋阿姨为什么要怕你牙呢？\n孩子:\t不知道。\n妈妈:\t我知道。\n妈妈:\t我讲了。",
        "post_context": "妈妈:\t他说这怪物的嘴巴这么大。\n妈妈:\t这么厉害。",
        "target": "妈妈:\t他____那是一个大怪物。",
        "option_yiwei": "以为",
        "option_juede": "觉得"
      },
      {
        "item": "103",
        "verb": "yiwei",
        "condition": "yiwei_unclear",
        "original_context": "孩子:\t流血了。\n大人:\t对。\n孩子:\t没有了。\n姐姐:\t没有了啦。\n姐姐:\t哇，这好漂亮诶。",
        "post_context": "大人:\t这是什么？\n姐姐:\t这边我来分吧。",
        "target": "姐姐:\t啊，我刚还____只有这一边呢。",
        "option_yiwei": "以为",
        "option_juede": "觉得"
      },
      {
        "item": "104",
        "verb": "yiwei",
        "condition": "yiwei_unclear",
        "original_context": "孩子:\t对呀！\n大人:\t然后这是谁？\n孩子:\t这是母鸡。\n大人:\t嗯。\n大人:\t母鸡看到小妹妹走过去啊。",
        "post_context": "大人:\t就好生气喔！\n大人:\t对着妹妹怎么样？",
        "target": "大人:\t____小妹妹要抓小鸡哦！",
        "option_yiwei": "以为",
        "option_juede": "觉得"
      },
      {
        "item": "105",
        "verb": "yiwei",
        "condition": "yiwei_unclear",
        "original_context": "孩子:\t小猫也很想去外面玩。\n妈妈:\t你看，这个主人贴了一个寻猫启事。\n妈妈:\t我给你念念啊。\n妈妈:\t寻猫启事，好不好？\n孩子:\t好。",
        "post_context": "妈妈:\t你一定是躲在某个角落偷偷的笑我。\n妈妈:\t这已经是第五天，你躲得太久了。",
        "target": "妈妈:\t你消失了。我____只是一如往常的躲猫猫游戏。",
        "option_yiwei": "以为",
        "option_juede": "觉得"
      },
      {
        "item": "106",
        "verb": "yiwei",
        "condition": "yiwei_unclear",
        "original_context": "妈妈:\t那么因为他们怕那些被赶走的坏人再来欺负他们。\n妈妈:\t所以就在门上面贴了这两个人的像。\n妈妈:\t贴着神荼跟郁垒的像。\n妈妈:\t贴在这边。\
        \n妈妈:\t那些坏人一看就不敢来了。",
        "post_context": "妈妈:\t不敢来了。\n妈妈:\t所以后来这两人贴在门上。",
        "target": "妈妈:\t就____这两个巨人还在这里。",
        "option_yiwei": "以为",
        "option_juede": "觉得"
      },
      {
        "item": "107",
        "verb": "yiwei",
        "condition": "yiwei_unclear",
        "original_context": "实验员:\t理头发又不会痛！\n实验员:\t不会痛。\n实验员:\t对不对？\n妈妈:\t你头发好长哎。\n妈妈:\t你看。",
        "post_context": "妈妈:\t人家都要问男的女的？\n孩子:\t我不要理头发。",
        "target": "妈妈:\t出去人家都____你是小女生。",
        "option_yiwei": "以为",
        "option_juede": "觉得"
      },
      {
        "item": "108",
        "verb": "yiwei",
        "condition": "yiwei_unclear",
        "original_context": "妈妈:\t那怎么办？\n妈妈:\t它要怎样还击？\n妈妈:\t它最大。\n妈妈:\t是不是？\n妈妈:\t其他动物都很小很小啊。",
        "post_context": "妈妈:\t那你是不是要表示一下友善。\n妈妈:\t说，“诶你看。”",
        "target": "妈妈:\t它们都____它会欺负它啊。",
        "option_yiwei": "以为",
        "option_juede": "觉得"
      },
      {
        "item": "109",
        "verb": "yiwei",
        "condition": "yiwei_unclear",
        "original_context": "大人:\t嗯。\n孩子:\t妈妈...妈妈。\n妈妈:\t啊。\n妈妈:\t你又把线弄成这个样子。\n大人:\t我在装这。",
        "post_context": "妈妈:\t呵呵。\n妈妈:\t好了。",
        "target": "妈妈:\t她____我走了。",
        "option_yiwei": "以为",
        "option_juede": "觉得"
      },
      {
        "item": "110",
        "verb": "yiwei",
        "condition": "yiwei_unclear",
        "original_context": "妈妈:\t快点！\n妈妈:\t我说要干吗。\n妈妈:\t我们去后面看。\n实验员:\t只是去看。\n妈妈:\t他叫我去后面看，水好满。",
        "post_context": "实验员:\t清掉。\n妈妈:\t没有。",
        "target": "实验员:\t我____他知道要叫你把他弄。",
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
        "original_context": "爸爸:\t妈妈照一张。\n爸爸:\t妈妈忘了戴帽子。\n妈妈:\t诶妈妈戴哪一顶比较可爱？\n妈妈:\t这顶吼？\n爸爸:\t对呀。",
        "post_context": "爸爸:\t妈妈要蹲着啦。\n妈妈:\t好。",
        "target": "妈妈:\t____这顶比较可爱。",
        "option_yiwei": "以为",
        "option_juede": "觉得"
      },
      {
        "item": "12",
        "verb": "juede",
        "condition": "juede_contrastive",
        "original_context": "老师:\t你看他的这个家是什么样的？\n孩子:\t这家很好。\n老师:\t很漂亮，是不是？\n孩子:\t我挺喜欢这个家的。\n老师:\t我也很喜欢。",
        "post_context": "老师:\t你们家是什么样的？\n孩子:\t我们家不漂亮。",
        "target": "老师:\t我____这个房子很漂亮。",
        "option_yiwei": "以为",
        "option_juede": "觉得"
      },
      {
        "item": "13",
        "verb": "juede",
        "condition": "juede_contrastive",
        "original_context": "老师:\t你猜猜看可能是什么？\n孩子:\t洗澡用的。\n老师:\t洗澡用的什么？\n老师:\t淋蓬头啊？\n孩子:\t对啊！",
        "post_context": "老师:\t那应该放在什么地方呢？\n孩子:\t放在洗澡的地方。",
        "target": "老师:\t哦,你____是洗澡用的淋蓬头。",
        "option_yiwei": "以为",
        "option_juede": "觉得"
      },
      {
        "item": "14",
        "verb": "juede",
        "condition": "juede_contrastive",
        "original_context": "妈妈:\t那这个咧。\n妈妈:\t这个在中间。\n妈妈:\t你对对看它要怎么拼。\n孩子:\t妈妈，这个我不会拼。\n妈妈:\t你可以把它放在这里。",
        "post_context": "妈妈:\t你要把它放进去看看吗？\n妈妈:\t对了吗？",
        "target": "妈妈:\t我____是在这里。",
        "option_yiwei": "以为",
        "option_juede": "觉得"
      },
      {
        "item": "15",
        "verb": "juede",
        "condition": "juede_contrastive",
        "original_context": "大人:\t哦，你蛮喜欢吃苹果的。\n孩子:\t是呀哈哈。\n大人:\t好我们看看下一页是什么。\n孩子:\t呵呵足球。\n孩子:\t哈哈好圆呐。",
        "post_context": "孩子:\t我家有的哈哈。\n大人:\t那你平时用它来做什么？",
        "target": "大人:\t哦，你____这是足球好圆呀。",
        "option_yiwei": "以为",
        "option_juede": "觉得"
      },
      {
        "item": "16",
        "verb": "juede",
        "condition": "juede_contrastive",
        "original_context": "妈妈:\t对啊！\n妈妈:\t可是这个盒子不是这个的吧！\n妈妈:\t是这样吗？\n孩子:\t压不下去啊！\n孩子:\t为什么刚刚那里就可以，这里就不行？",
        "post_context": "妈妈:\t妈妈试一下喔!\n妈妈:\t先...先这样子。",
        "target": "妈妈:\t我____盒子好像不是这一个。",
        "option_yiwei": "以为",
        "option_juede": "觉得"
      },
      {
        "item": "17",
        "verb": "juede",
        "condition": "juede_contrastive",
        "original_context": "妈妈:\t是蚊子吗？\n孩子:\t不是。\n妈妈:\t那是什么？\n孩子:\t瓢虫。\n妈妈:\t是瓢虫喔。",
        "post_context": "妈妈:\t蜘蛛没有回答。\n妈妈:\t他正忙着织网呢。",
        "target": "妈妈:\t你____是瓢虫喔。",
        "option_yiwei": "以为",
        "option_juede": "觉得"
      },
      {
        "item": "18",
        "verb": "juede",
        "condition": "juede_contrastive",
        "original_context": "孩子:\t就这个字啊。\n妈妈:\t对。\n妈妈:\t地下室也有大妖怪。\n妈妈:\t你看。\n妈妈:\t地下室也有大妖怪。",
        "post_context": "妈妈:\t外面的垃圾桶也有大妖怪。\n妈妈:\t他都不敢去丢垃圾。",
        "target": "妈妈:\t他____地下室里也有大妖怪。",
        "option_yiwei": "以为",
        "option_juede": "觉得"
      },
      {
        "item": "19",
        "verb": "juede",
        "condition": "juede_contrastive",
        "original_context": "孩子:\t唉呦两眼。\n妈妈:\t那不是眼睛啦。\n孩子:\t很像啦。\n妈妈:\t好，很像，对啦。\n妈妈:\t很像。",
        "post_context": "孩子:\t三眼怪。\n妈妈:\t嗯。",
        "target": "妈妈:\t刚刚我看的也____很像。",
        "option_yiwei": "以为",
        "option_juede": "觉得"
      },
      {
        "item": "20",
        "verb": "juede",
        "condition": "juede_contrastive",
        "original_context": "孩子:\t还有可以舀水的。\n老师:\t这么多呀！\n老师:\t太好玩了。\n老师:\t我们来看一看这么漂亮的房子是谁的呀？\n孩子:\t小狐狸的家。",
        "post_context": "孩子:\t小狐狸，小狐狸爸爸，小狐狸妈妈，小狐狸宝宝。\n老师:\t小动物的家有几口人呢？",
        "target": "老师:\t你____是小狐狸的家，我们看看。",
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
        "original_context": "妈妈:\t你看这个是不是像一个鸡蛋？\n妈妈:\t要怎么拼呢？\n孩子:\t这样拼！\n妈妈:\t这样拼。\n妈妈:\t对啊。",
        "post_context": "妈妈:\t好。\n妈妈:\t啊我知道了。",
        "target": "妈妈:\t我也____这个拼图有一点点难。",
        "option_yiwei": "以为",
        "option_juede": "觉得"
      },
      {
        "item": "112",
        "verb": "juede",
        "condition": "juede_unclear",
        "original_context": "妈妈:\t你怎么这么厉害啊。\n妈妈:\t从哪里开的啊？\n妈妈:\t哦用力开。\n姐姐:\t打开。\n妈妈:\t是这样吗？",
        "post_context": "妈妈:\t你确定是用力开吗？(笑)\n妈妈:\t你喜欢玩什么？",
        "target": "妈妈:\t我____你弄错了。",
        "option_yiwei": "以为",
        "option_juede": "觉得"
      },
      {
        "item": "113",
        "verb": "juede",
        "condition": "juede_unclear",
        "original_context": "妈妈:\t为什么这是一个阿姨？\n孩子:\t因为她叫阿姨。\n妈妈:\t那好吧，这是一个阿姨。\n妈妈:\t这个阿姨戴着一个头盔对吧？\n妈妈:\t骑着摩托车斜挎着一个包。",
        "post_context": "妈妈:\t她可能是送信呀送报纸对吧？\n妈妈:\t然后你看这阿姨笑眯眯地蹲下来，看到了一个什么？",
        "target": "妈妈:\t我____她可能是一个邮递员阿姨。",
        "option_yiwei": "以为",
        "option_juede": "觉得"
      },
      {
        "item": "114",
        "verb": "juede",
        "condition": "juede_unclear",
        "original_context": "妈妈:\t等一下就知道了。\n妈妈:\t欸，你要不要...你看！\n妈妈:\t我盖得很高。\n妈妈:\t你要不要再帮我？\n孩子:\t你看！",
        "post_context": "孩子:\t不会。\n妈妈:\t真的吗？",
        "target": "妈妈:\t可是我____你的被风吹一下就倒了。",
        "option_yiwei": "以为",
        "option_juede": "觉得"
      },
      {
        "item": "115",
        "verb": "juede",
        "condition": "juede_unclear",
        "original_context": "孩子:\t佩奇就不梳了吧！\n老师:\t那你给乔治梳一下。\n孩子:\t好了，这个梳子放哪里我不知道。\n老师:\t你自己想把它放在哪里？\n孩子:\t你给我说说嘛！",
        "post_context": "老师:\t然后你在这里面找一找。\n孩子:\t这个里面有楼梯。",
        "target": "老师:\t我____它可以放在这里面。",
        "option_yiwei": "以为",
        "option_juede": "觉得"
      },
      {
        "item": "116",
        "verb": "juede",
        "condition": "juede_unclear",
        "original_context": "妈妈:\t哇这些大树漂亮吗？\n孩子:\t有果树和柳树。\n妈妈:\t这有点像我们那次去新加坡看到的树。\n孩子:\t这种椰树。\n妈妈:\t对对对对对。",
        "post_context": "孩子:\t想要大树需要种子。\n妈妈:\t哇这么多啊。",
        "target": "妈妈:\t这种我____我也认不出来。",
        "option_yiwei": "以为",
        "option_juede": "觉得"
      },
      {
        "item": "117",
        "verb": "juede",
        "condition": "juede_unclear",
        "original_context": "孩子:\t哪有小盘子呐？\n老师:\t没有小盘子，快想想办法吧！\n孩子:\t嗯，我知道了。\n老师:\t嗯。\n孩子:\t我看看这里边有没有。",
        "post_context": "孩子:\t把它放在这个。\n老师:\t哦,用这个来当盘子啊？",
        "target": "老师:\t你____在抽屉里啊！",
        "option_yiwei": "以为",
        "option_juede": "觉得"
      },
      {
        "item": "118",
        "verb": "juede",
        "condition": "juede_unclear",
        "original_context": "妈妈:\t它喜欢抓海里的鱼吃呗。\n孩子:\t它喜欢...喜欢吃鲨鱼？\n妈妈:\t鲨鱼啊？\n妈妈:\t不知道哎。\n妈妈:\t鲨鱼是不是太生猛了一点啊？",
        "post_context": "孩子:\t北极熊是会成为鲨鱼的晚餐。\n妈妈:\t但是北极还有一种动物。",
        "target": "妈妈:\t我____北极熊有可能追不上它。",
        "option_yiwei": "以为",
        "option_juede": "觉得"
      },
      {
        "item": "119",
        "verb": "juede",
        "condition": "juede_unclear",
        "original_context": "孩子:\t没...没有水啊。\n妈妈:\t马上就看完了。\n妈妈:\t我们过来看一下。\n孩子:\t我还想多喝一点水。\n妈妈:\t好。",
        "post_context": "妈妈:\t你看。\n妈妈:\t它和主人在一起。",
        "target": "妈妈:\t妈妈____这个猫咪它好幸福啊。",
        "option_yiwei": "以为",
        "option_juede": "觉得"
      },
      {
        "item": "120",
        "verb": "juede",
        "condition": "juede_unclear",
        "original_context": "妈妈:\t他总是在想有没有怪兽，对吧？\n孩子:\t嗯。\n妈妈:\t他把泡沫弄在头发上。\n孩子:\t过一会儿他就去看看。\n妈妈:\t哦。",
        "post_context": "孩子:\t对。\n妈妈:\t洗完澡他回房间呢。",
        "target": "妈妈:\t他总是____怪兽在旁边，对吧？",
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
        "original_context": "妈妈:\t这是拼成一个什么形状啊？\n孩子:\t圆形。\n妈妈:\t对了。\n妈妈:\t这个实际上就是两个半圆形拼在一起，对吧？\n孩子:\t对。",
        "post_context": "妈妈:\t这个叫六边形。\n孩子:\t六边形。",
        "target": "妈妈:\t另外这个我们____没有看过。",
        "option_yiwei": "以前",
        "option_juede": "以后"
      },
      {
        "item": "902",
        "verb": "filler",
        "condition": "filler",
        "original_context": "妈妈:\t画什么？\n孩子:\t鱼。\n妈妈:\t嗯，自己画！\n孩子:\t妈妈，你给我画。\n妈妈:\t你画。",
        "post_context": "妈妈:\t还画得挺好的。\n妈妈:\t现在怎么不会了？",
        "target": "妈妈:\t你不是____画过鱼吗？",
        "option_yiwei": "以前",
        "option_juede": "以后"
      },
      {
        "item": "903",
        "verb": "filler",
        "condition": "filler",
        "original_context": "妈妈:\t摆摆好，放整齐。\n孩子:\t这两个是一样的。\n妈妈:\t噢，一样的。\n妈妈:\t那盒子里装的是什么？\n妈妈:\t这是什么？",
        "post_context": "孩子:\t这个 。\n妈妈:\t真好玩。",
        "target": "妈妈:\t玩过了____要收起来。",
        "option_yiwei": "以后",
        "option_juede": "以前"
      },
      {
        "item": "904",
        "verb": "filler",
        "condition": "filler",
        "original_context": "妈妈:\t你想搭什么？\n孩子:\t想搭一座小桥！\n妈妈:\t想搭小桥。\n妈妈:\t搭这个也好。\n妈妈:\t那你来搭吧！",
        "post_context": "妈妈:\t就像建大楼一样。\n妈妈:\t下面建得很牢固上面才能搭高。",
        "target": "妈妈:\t底下要搭稳了____才能把上面搭高。",
        "option_yiwei": "以后",
        "option_juede": "以前"
      },
      {
        "item": "905",
        "verb": "filler",
        "condition": "filler",
        "original_context": "妈妈:\t我们走了好久了。\n孩子:\t这里好远哦。\n妈妈:\t这里怎么这么安静？\n孩子:\t都没有车。\n妈妈:\t空气也很好。",
        "post_context": "孩子:\t我们可以多待一会儿吗？\n妈妈:\t好。",
        "target": "爸爸:\t感觉像个世外____。",
        "option_yiwei": "桃源",
        "option_juede": "花园"
      },
      {
        "item": "906",
        "verb": "filler",
        "condition": "filler",
        "original_context": "妈妈:\t阳台这边终于收拾干净了。\n爸爸:\t确实看着清爽多了。\n爸爸:\t不过总觉得少了点生气。\n爸爸:\t要不要我把那盆绿萝挪过来？\n妈妈:\t放在窗边？",
        "post_context": "孩子:\t现在这里好漂亮。\n妈妈:\t嗯，就这样吧。",
        "target": "妈妈:\t确实，这样一摆，真有点画龙____。",
        "option_yiwei": "点睛",
        "option_juede": "加眼"
      },
      {
        "item": "907",
        "verb": "filler",
        "condition": "filler",
        "original_context": "妈妈:\t你看，\n妈妈:\t这个村长说得挺热闹的。\n妈妈:\t计划写了一大堆。\n妈妈:\t可事情始终没开始。\n妈妈:\t村民们都不乐意了。",
        "post_context": "孩子:\t嗯。\n孩子:\t然后呢？",
        "target": "妈妈:\t全是纸上____。",
        "option_yiwei": "谈兵",
        "option_juede": "说兵"
      },
      {
        "item": "908",
        "verb": "filler",
        "condition": "filler",
        "original_context": "爸爸:\t小明刚才帮你把书搬过来，对吗？\n孩子:\t对。\n爸爸:\t那你为什么转身就走了？\n孩子:\t我想去玩积木。\n爸爸:\t我知道。",
        "post_context": "孩子:\t我不是故意的。\n爸爸:\t那我们回去一起帮他一下吧。",
        "target": "爸爸:\t但是我们不能过河____呀。",
        "option_yiwei": "拆桥",
        "option_juede": "拆船"
      },
      {
        "item": "909",
        "verb": "filler",
        "condition": "filler",
        "original_context": "实验员:\t这张画已经很好看了。\n孩子:\t我还想再加点。\n实验员:\t加什么？\n孩子:\t一些亮片。\n实验员:\t可我觉得不加刚刚好。",
        "post_context": "孩子:\t可是我喜欢。\n孩子:\t亮亮的。",
        "target": "实验员:\t加了反而有点____添足。",
        "option_yiwei": "画蛇",
        "option_juede": "画龙"
      },
      {
        "item": "910",
        "verb": "filler",
        "condition": "filler",
        "original_context": "妈妈:\t我们可以玩什么游戏？\n孩子:\t医生游戏，要生病吃药。\n妈妈:\t谁是病人？\n孩子:\t你。\n妈妈:\t我是病人啊，要吃药了哇。",
        "post_context": "孩子:\t不行。\n孩子:\t我还没看呢。",
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
    exp.structure=["i0", "reminder",
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
