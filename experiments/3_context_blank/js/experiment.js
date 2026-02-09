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
        var context = "妈妈:\t小球滚到那里去了耶。\n孩子:\t哪里？\n妈妈:\t沙发底下。\n妈妈:\t好远哦。\n孩子:\t那怎么办？";
        var target = "妈妈:\t你可以帮妈妈把小球拿____吗？"
        var post_context = "孩子:\t不好。\n孩子:\t不要。";
        var full_sent = context + "\n" + target + "\n" + post_context
        $(".target").html(full_sent);

        exp.response = undefined;
        $('#practice_1_input').val(''); // clear the previous response
      },

      button : function() {
        exp.response = $('#practice_1_input').val();
        console.log("response", exp.response);

        if (exp.response == undefined || exp.response == "" || exp.response.trim() == "") {
          $(".forced_choice_err").show();
        } else {
          this.log_responses();
          _stream.apply(this);
        }
        // console.log(exp.response);
      },
      
      log_responses : function() {
        console.log("logged response: "+ exp.response);
        exp.data_trials.push({
          "trial_num" : 0,
          "item_id" : "301",
          "block_id" : "practice",
          "condition" : "practice_1",
          "verb": "NA",
          "response" : exp.response,
        });
  
      }
    });

    slides.practice_2 = slide({
        name : "practice_2",
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
          var context = "\n妈妈:\t妈妈给你讲个故事。\n孩子:\t好。\n妈妈:\t有一天夜里呢，刮了大风。\n妈妈:\t到天亮的时候，风才停下。\n妈妈:\t沼泽地的村民们纷纷从各自家里钻了出来。";
          var target = "妈妈:\t修补被大风____了的房屋？"
          var post_context = "妈妈:\t它们就修房子。\n妈妈:\t蜘蛛阿尔丁昨天刚结的几个新网也破了几个大窟窿。";
          var full_sent = context + "\n" + target + "\n" + post_context
          $(".target").html(full_sent);

          exp.response = undefined;
          $('#practice_2_input').val(''); // clear the previous response
        },
        button : function() {
          exp.response = $('#practice_2_input').val();
          console.log("response", exp.response);

          if (exp.response == undefined || exp.response == "" || exp.response.trim() == "") {
            $(".forced_choice_err").show();
          } else {
            this.log_responses();
            _stream.apply(this);
          }
          // console.log(exp.response);
        },
       
        log_responses : function() {
          console.log("logged response: "+ exp.response);
          exp.data_trials.push({
            "trial_num" : 0,
            "item_id" : "302",
            "block_id" : "practice",
            "condition" : "practice_2",
            "verb": "NA",
            "response" : exp.response,
          });
    
        }
      });


    slides.practice_3 = slide({
      name : "practice_3",
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
        var context = "妈妈:\t你画太阳吧。\n孩子:\t你帮我画房子。\n妈妈:\t我帮你画房子啊。\n孩子:\t因为我不会画嘛。\n妈妈:\t不会画没关系。";
        var target = "妈妈:\t你可以____这个颜色涂在这里。"
        var post_context = "孩子:\t不好。\n孩子:\t不要。";
        var full_sent = context + "\n" + target + "\n" + post_context
        $(".target").html(full_sent);

        exp.response = undefined;
        $('#practice_3_input').val(''); // clear the previous response
      },
      button : function() {
        exp.response = $('#practice_3_input').val();
        console.log("response", exp.response);

        if (exp.response == undefined || exp.response == "" || exp.response.trim() == "") {
          $(".forced_choice_err").show();
        } else {
          this.log_responses();
          _stream.apply(this);
        }
        // console.log(exp.response);
      },
      
      log_responses : function() {
        console.log("logged response: "+ exp.response);
        exp.data_trials.push({
          "trial_num" : 0,
          "item_id" : "303",
          "block_id" : "practice",
          "condition" : "practice_3",
          "verb": "NA",
          "response" : exp.response,
        });
  
        }
      });
  
    slides.practice_4 = slide({
      name : "practice_4",
  
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
          var context = "妈妈:\t我们有小推车了！\n妈妈:\t你看那边有什么?\n孩子:\t蛋蛋。\n妈妈:\t蛋蛋哦！\n妈妈:\t你很喜欢蛋蛋诶。";
          var target = "妈妈:\t那要不要把小车推____装蛋蛋？"
          var post_context = "妈妈:\t要不要装蛋？\n孩子:\t要！";
          var full_sent = context + "\n" + target + "\n" + post_context
          $(".target").html(full_sent);

          exp.response = undefined;
          $('#practice_4_input').val(''); // clear the previous response
      },
      button : function() {
          exp.response = $('#practice_4_input').val();
          console.log("response", exp.response);

          if (exp.response == undefined || exp.response == "" || exp.response.trim() == "") {
              $(".forced_choice_err").show();
          } else {
            this.log_responses();
            _stream.apply(this);
          }
          // console.log(exp.response);
      },
      log_responses : function() {
        console.log("logged response: " + exp.response)
        exp.data_trials.push({
          "trial_num" : 0,
          "item_id" : "304",
          "block_id" : "practice",
          "condition" : "practice_4",
          "verb": "NA",
          "response" : exp.response,
        });
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
           
            $('#response_input').val(''); // clear the previous response
            exp.response = undefined; // remove the previous selection

            $(".continue_button").show(); // show the continue button

            console.log(this.stim); 
  
        },
        
        button : function() {
          exp.response = $('#response_input').val();
          console.log("response", exp.response);

          if (exp.response == undefined || exp.response == "" || exp.response.trim() == "") {
              $(".forced_choice_err").show();
          } else {
            this.log_responses();
            _stream.apply(this); //use exp.go() if and only if there is no "present" data.
          }
          // console.log(exp.response);
        },


        log_responses : function() {
          console.log("logged response: " + exp.response)
          exp.data_trials.push({
              "trial_num" : order,
              "item_id" : this.stim.item,
              "block_id" : this.stim.block_id,
              "condition" : this.stim.condition,
              "verb": this.stim.verb,
              "response" : exp.response,
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
        "target": "实验员:\t我____是要那个大车子。",
        "option_yiwei": "以为",
        "option_juede": "觉得"
      },
      {
        "item": "4",
        "verb": "yiwei",
        "condition": "yiwei_contrastive",
        "original_context": "孩子:\t咦，他才不是小哥哥。\n妈妈:\t啊，那不是小哥哥。\n妈妈:\t我讲错了。\n妈妈:\t你看，你一直摸我这里。\n妈妈:\t我就忘记。",
        "post_context": "妈妈:\t所以你要专心啊。\n妈妈:\t不然我都会忘记啊。",
        "target": "妈妈:\t我____他是小哥哥。",
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
        "original_context": "孩子:\t最后黑棋一般只要吃八十七个空，白棋一般吃六十几个空。\n实验员:\t为什么还不一样？\n孩子:\t因为这样才公平。\n孩子:\t因为白棋的棋子比黑棋的棋子少呀。\n实验员:\t哦这样啊。",
        "post_context": "实验员:\t为什么要少一点？\n孩子:\t因为这个事儿我也不太知道呀。",
        "target": "实验员:\t我___是一样的。",
        "option_yiwei": "以为",
        "option_juede": "觉得"
      },
      {
        "item": "10",
        "verb": "yiwei",
        "condition": "yiwei_contrastive",
        "original_context": "实验员:\t这个。(手指着画)\n孩子:\t画的形状都很像哟。(手指着画)\n实验员:\t这是什么？(手指着画)\n孩子:\t这是鸟。\n实验员:\t这是鸟？",
        "post_context": "实验员:\t啊。\n孩子:\t这我画的小鱼和水。",
        "target": "实验员:\t哦，我____是人呢。",
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
        "original_context": "孩子:\t小猫也很想去外面玩。\n妈妈:\t你看，这个主人贴了一个寻猫启事。\n妈妈:\t我给你念念啊，寻猫启事，好不好？\n孩子:\t好。\n妈妈:\t你消失了。",
        "post_context": "妈妈:\t你一定是躲在某个角落偷偷的笑我。\n妈妈:\t这已经是第五天，你躲得太久了。",
        "target": "妈妈:\t我____只是一如往常的躲猫猫游戏。",
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
        "original_context": "妈妈:\t哦这里还有一个。\n妈妈:\t我们读读这里,会更有感触。\n妈妈:\t一开始只是单位友人收养的流浪猫出走时的失落。\n妈妈:\t失落是潜在心底的一股浮流。\n妈妈:\t猫默默的走进你的一生。",
        "post_context": "妈妈:\t它又出其不意的走出你的生命。\n妈妈:\t它的去向、是不是平安，一无所知。",
        "target": "妈妈:\t当你____是你的责任，",
        "option_yiwei": "以为",
        "option_juede": "觉得"
      },
      {
        "item": "109",
        "verb": "yiwei",
        "condition": "yiwei_unclear",
        "original_context": "大人:\t嗯。\n孩子:\t妈妈...妈妈。\n妈妈:\t啊。\n妈妈:\t你又把线弄成这个样子。\n大人:\t我在装这个。",
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
        "target": "实验员:\t我____他知道要叫你帮他弄...",
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
        "target": "老师:\t哦，你____是洗澡用的淋蓬头。",
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
        "original_context": "妈妈:\t山羊太太右手拎了一个东西。\n妈妈:\t拎不动了。\n妈妈:\t他就帮山羊太太拎...拎这个水果篮子。\n妈妈:\t对不对啊？\n孩子:\t对啊。",
        "post_context": "妈妈:\t他老是愿意帮助别人。\n妈妈:\t这一页画的是什么呀？",
        "target": "妈妈:\t我____我好喜欢小狐狸。",
        "option_yiwei": "以为",
        "option_juede": "觉得"
      },
      {
        "item": "113",
        "verb": "juede",
        "condition": "juede_unclear",
        "original_context": "妈妈:\t那怎么处理这粒种子呢？\n妈妈:\t蜘蛛问老蚯蚓。\n妈妈:\t老蚯蚓若有所思。\n妈妈:\t“把他种起来，\n妈妈:\t说不定还能结出什么美味的果子来呢！”",
        "post_context": "妈妈:\t然后就把这粒种子种到地里去。\n妈妈:\t蟋蟀还在种子的窝边用绿叶搭起一个帐篷。",
        "target": "妈妈:\t大家____这个主意不错。",
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
        "post_context": "孩子:\t把它放在这个...\n老师:\t哦,用这个来当盘子啊？",
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
        "original_context": "妈妈:\t你画了什么呀？\n孩子:\t小猫！\n妈妈:\t刚开始画得挺好的呀，还涂了颜色。\n妈妈:\t怎么后面就只画了一个圈？\n孩子:\t我不想画了。",
        "post_context": "孩子:\t我画累了。\n妈妈:\t那我们休息一下，不要就这么放弃，好不好？",
        "target": "妈妈:\t那你这不是____蛇尾嘛。",
        "option_yiwei": "虎头",
        "option_juede": "猫头"
      },
      {
        "item": "902",
        "verb": "filler",
        "condition": "filler",
        "original_context": "妈妈:\t你刚刚是不是去拿饼干了？\n孩子:\t没有。\n妈妈:\t那你嘴上怎么全是饼干渣？\n孩子:\t（捂住嘴）我没有吃。\n妈妈:\t你捂住嘴不让我看，就当妈妈不知道了？",
        "post_context": "孩子:\t那我现在不能吃了吗？\n妈妈:\t可以，但先说实话。",
        "target": "妈妈:\t这不是有点掩耳____吗。",
        "option_yiwei": "盗铃",
        "option_juede": "偷鼓"
      },
      {
        "item": "903",
        "verb": "filler",
        "condition": "filler",
        "original_context": "孩子:\t我不收玩具了。\n妈妈:\t怎么啦？\n孩子:\t我等它自己回盒子。\n妈妈:\t玩具会自己走回去吗？\n孩子:\t不会，可是我在等。",
        "post_context": "孩子:\t那要怎么办？\n妈妈:\t我们一起收，很快就好了。",
        "target": "妈妈:\t光等着不动，守株____吗？",
        "option_yiwei": "待兔",
        "option_juede": "待免"
      },
      {
        "item": "904",
        "verb": "filler",
        "condition": "filler",
        "original_context": "孩子:\t我给花多浇点水，它就会长得快。\n妈妈:\t水已经够了。\n孩子:\t那我再倒一点。\n妈妈:\t倒太多会烂根的。\n孩子:\t我想让它快点长大。",
        "post_context": "孩子:\t那怎么办？\n妈妈:\t每天一点点就好。",
        "target": "妈妈:\t这样硬来，其实是拔苗____。",
        "option_yiwei": "助长",
        "option_juede": "帮忙"
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
        "original_context": "妈妈:\t阳台这边终于收拾干净了。\n爸爸:\t确实看着清爽多了。\n爸爸:\t不过看起来少了点生气。\n爸爸:\t要不要我把那盆绿萝挪过来？\n妈妈:\t放在窗边？",
        "post_context": "孩子:\t现在这里好漂亮。\n妈妈:\t嗯，就这样吧。",
        "target": "妈妈:\t确实，这样一摆真有点画龙____。",
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
        "original_context": "实验员:\t这张画已经很好看了。\n孩子:\t我还想再加点。\n实验员:\t加什么？\n孩子:\t一些亮片。\n实验员:\t可不加挺好的呀。",
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
    exp.structure=["i0", "instruction", "reminder",
    "practice_1", "practice_2", 
    "practice_3", "practice_4",
    "last_reminder", "block1", 'questionaire', 'finished'];
    // console.log(exp.structure);

    exp.data_trials = [];
    //make corresponding slides:
    exp.slides = make_slides(exp);

    //   exp.nQs = utils.get_exp_length(); //this does not work if there are stacks of stims (but does work for an experiment with this structure)
                        //relies on structure and slides being defined
                        
    exp.nQs = 1 + 1 + 4 + 1 + 50 + 2; 
    $(".nQs").html(exp.nQs);

    $('.slide').hide(); //hide everything

    $("#start_button").click(function() {
        exp.go();
    });

    exp.go(); //show first slide
}
