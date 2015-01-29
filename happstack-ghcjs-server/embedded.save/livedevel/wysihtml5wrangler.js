function clickToEditBad(srcsel) {
   var flag = true;
   return (function () {
      var source = $(srcsel);
      if (flag) {
        // copy the html contents of the src
        // then replace it with a textarea with that html.
        source.wrapInner("<textarea id='foo'></textarea>");
        $("#foo").wysihtml5();
	flag = false;
      } else {
	var t = edt_wysihtml5.getValue();
	source.html(t);
	source.show();
	editor_outside.hide();
	flag = true;
      }
   });
}

var global;
function clickToEdit() {
   console.log('wysihtml5wrangler.js');
   return (function (e) {
       var source = $(this);
       console.log('this');
       console.log(this);
       // copy the html contents of the src
       // then replace it with a textarea with that html.
       var t = source.html();
       console.log('e');
       console.log(e);
       console.log('t');
       console.log(t);
       source.empty();
       var rapper='<textarea style="width:100%; height:100%; background-color:transparent; border:0"/>';
       var wrapper=$('<textarea style="width:100%; height:100%; background-color:transparent; border:0"/>');
       wrapper.html(t);
       console.log('wrapper');
       console.log(wrapper);
       console.log('source');
       console.log(source);
       var b = $("<button type='button'>Done</button>");
       source.append( b );
       b.on('click',function(e) { restoreClickToEdit(source); });
       source.append( wrapper);
       source.removeClass( "clickToEdit" );
       source.addClass( "activeEditor" );
       // $(".appraisalTopDiv").off('click', source);
       global = wrapper.wysihtml5();
       return (source);
   });
}



function restoreClickToEdit(src) {
    // var source=$(".appraisalWysiwyg,.activeEditor");
    var source=$(src);
    var t = source.find("textarea").val();
    source.empty();
    source.removeClass( "activeEditor" );
    source.addClass( "clickToEdit" );
    source.html(t);
    console.log(t);
}


function testLive() {
   var readonly = '<div contentEditable="false" class="wysihtml5-uneditable-container" style="padding: 10px;">This content is not editable</div>'
    $(".appraisalTopDiv").append("<div class='appraisalWysiwyg clickToEdit'><b>Goodbye</b><i>cruel world!</i><p>" + readonly + "</div>");
}


function foo () { 
    $(".appraisalTopDiv").on('click', ".clickToEdit", clickToEdit());
}

$(document).ready(foo);