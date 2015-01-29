
var appraisalDebug = (function() {
    var flag = false;
    var obj = { set: function(f) { flag = f; return flag; },
		get: function(f) { return flag; }
	      }
    return obj;
})();



{ console.log('livedevel.js active'); }

// Make the whole area around the text a button to activate the editor.
// Make the titlebar a button, too.
// Make the titlebar a button, too.

function buttonFromScratch(sel) {
    var d = $(sel);
//    d.addClass("buttonFromScratch");
    moo(sel);
    d.on('click', function() { console.log('click'); });
}

function moo(sel) {
    $(sel).mouseover(
        function(e) {
	    evname = "mouseover";
	    console.log(evname + ' before: ' + $(this).attr('class'));
            e.stopPropagation();
            $(this).removeClass("buttonNormal");
            $(this).addClass("buttonHover");
	    console.log(evname + ' after: ' + $(this).attr('class'));
        }).mouseout(
        function() {
	    evname = "mouseout";
	    console.log(evname + ' before: ' + $(this).attr('class'));
            $(this).removeClass("buttonHover");
            $(this).addClass("buttonNormal");
	    console.log(evname + ' after: ' + $(this).attr('class'));
        }).mousedown(
        function() {
	    evname = "mousedown";
	    console.log(evname + ' before: ' + $(this).attr('class'));
            $(this).removeClass("buttonHover");
            $(this).addClass("buttonActive");
	    console.log(evname + ' after: ' + $(this).attr('class'));
        }).mouseup(
        function() {  //bugger, is the mouse still in or not?  Assume so, but find out how to check.
	    evname = "mouseup";
	    console.log(evname + ' before: ' + $(this).attr('class'));
            $(this).removeClass("buttonActive");
            $(this).addClass("buttonHover");
	    console.log(evname + ' after: ' + $(this).attr('class'));
	});
}

function live() {
    var top = ".appraisalTopDiv";
    $(top).append('<div id="somediv">Before.&nbsp;</div><div id="insideDiv">This is the inside div.</div>After');
    $(top).append('<div>Before.&nbsp;</div><div id="insideDiv2">This is the second inside div.</div>After');
    buttonFromScratch(top);
    buttonFromScratch("#insideDiv");
    buttonFromScratch("#insideDiv2");

    $(top).append('<p><button id="abbrevButton" type="button"><b>Abbreviation</b> as <i>button.</i></button>');
    $(top).append('<div id="test1">this is the outer div with a button in it.<button type="button">Outer button.</button> Now there is more text here.</div>');
    $('#abbrevButton').on('click', function(e) { e.stopPropagation(); console.log('abbrev'); });
}

live();

