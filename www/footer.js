

var CLIPBOARD = "";
var tablesToExcel = (function() {
  var uri = 'data:application/vnd.ms-excel;base64,'
  , tmplWorkbookXML = '<?xml version="1.0"?><?mso-application progid="Excel.Sheet"?><Workbook xmlns="urn:schemas-microsoft-com:office:spreadsheet" xmlns:ss="urn:schemas-microsoft-com:office:spreadsheet">'
  + '<DocumentProperties xmlns="urn:schemas-microsoft-com:office:office"><Author>Axel Richter</Author><Created>{created}</Created></DocumentProperties>'
  + '<Styles>'
  + '<Style ss:ID="Currency"><NumberFormat ss:Format="Currency"></NumberFormat></Style>'
  + '<Style ss:ID="Date"><NumberFormat ss:Format="Medium Date"></NumberFormat></Style>'
  + '</Styles>' 
  + '{worksheets}</Workbook>'
  , tmplWorksheetXML = '<Worksheet ss:Name="{nameWS}"><Table>{rows}</Table></Worksheet>'
  , tmplCellXML = '<Cell{attributeStyleID}{attributeFormula}><Data ss:Type="{nameType}">{data}</Data></Cell>'
  , base64 = function(s) { return window.btoa(unescape(encodeURIComponent(s))) }
  , format = function(s, c) { return s.replace(/{(\w+)}/g, function(m, p) { return c[p]; }) }
  return function(tables, wsnames, wbname, appname) {
    var ctx = "";
    var workbookXML = "";
    var worksheetsXML = "";
    var rowsXML = "";
    
    for (var i = 0; i < tables.length; i++) {
      if (!tables[i].nodeType) tables[i] = document.getElementById(tables[i]);
      for (var j = 0; j < tables[i].rows.length; j++) {
        rowsXML += '<Row>'
        for (var k = 0; k < tables[i].rows[j].cells.length; k++) {
          var dataType = tables[i].rows[j].cells[k].getAttribute("data-type");
          var dataStyle = tables[i].rows[j].cells[k].getAttribute("data-style");
          var dataValue = tables[i].rows[j].cells[k].getAttribute("data-value");
          dataValue = (dataValue)?dataValue:tables[i].rows[j].cells[k].innerHTML;
          var dataFormula = tables[i].rows[j].cells[k].getAttribute("data-formula");
          dataFormula = (dataFormula)?dataFormula:(appname=='Calc' && dataType=='DateTime')?dataValue:null;
          ctx = {  attributeStyleID: (dataStyle=='Currency' || dataStyle=='Date')?' ss:StyleID="'+dataStyle+'"':''
            , nameType: (dataType=='Number' || dataType=='DateTime' || dataType=='Boolean' || dataType=='Error')?dataType:'String'
            , data: (dataFormula)?'':dataValue
            , attributeFormula: (dataFormula)?' ss:Formula="'+dataFormula+'"':''
          };
          rowsXML += format(tmplCellXML, ctx);
        }
        rowsXML += '</Row>'
      }
      ctx = {rows: rowsXML, nameWS: wsnames[i] || 'Sheet' + i};
      worksheetsXML += format(tmplWorksheetXML, ctx);
      rowsXML = "";
    }
    
    ctx = {created: (new Date()).getTime(), worksheets: worksheetsXML};
    workbookXML = format(tmplWorkbookXML, ctx);
    
    console.log(workbookXML);
    
    var link = document.createElement("A");
    link.href = uri + base64(workbookXML);
    link.download = wbname || 'Workbook.xls';
    link.target = '_blank';
    document.body.appendChild(link);
    link.click();
    document.body.removeChild(link);
  }
})();


$(function () {
    $(".allownumericwithoutdecimal").on("keypress keyup blur", function (event) {
        $(this).val($(this).val().replace(/^\d{1}.+/, "")); // /[^\d].+/
        if ((event.which <= 48 || event.which > 51)) {
            event.preventDefault();
        }
    });
    $('#file1').on('click',function(){
      this.value=null;
    })
    
    $(document).on("click",  "#btnRmfile", function(e) { 
      event.preventDefault();
     //event.stopPropagation();
     e.isPropagationStopped;
     $('#btnRmfile').remove();
      $('#file1')[0].value = null; 
      $('#file1').parents('div')[0].childNodes[3].value = null;
      $('#file1_progress').css({visibility:'hidden'});
      $('#file1_progress .progress-bar').css({visibility:'hidden'});
      });
    $("#barchart").css({"display":"inline-block"});
   $("#sentimentcloud").css({"display":"inline-block"});
    //$("#cluster").css({"width":"auto","overflow-x":"scroll"})
    $('#file1').change(function() {
       if($('#file1').val() !=""){
      $('#btnRmfile').remove();
      $('<i class="fa fa-trash-o" id="btnRmfile" style="cursor:pointer;" title="Remove File"></i>').prependTo($('#file1').parent());
    } else {
       $('#btnRmfile').remove();
    }
      //if($('#submit').is(':disabled')){
      $("#collapseOne").collapse("show");
        Shiny.onInputChange('flg', 0)
        
        $("#wcContainer").css({"display":"none"});
        //$("#plot")[0].innerHTML="";
        d3.select("#plot svg").remove();
        $("#selWord")[0].innerText="";
        $("#wc2Container").css({"display":"none"});
        d3.select("#plot2 svg").remove();
        $("#tbl").css({"display":"none"});
        $("#corPlot").css({"display":"none"});
        $("#txtFilter").val("");
        Shiny.onInputChange('txtFilter',"");
        
         //var $select = $('#words').selectize();
 //var control = $select[0].selectize;
 //control.clear();
        //Shiny.onInputChange('words',"");
         $("#rmWrds").attr('checked', false)
        //Shiny.onInputChange('rmWrds',false);
    //}
        var filename = $('#file1').val();
        $('#select_file').html(filename);
    
        if (filename == "") {
            $("#file1_progress").css({ "height": "0px" });
            $("#file1_progress .progress-bar").css({ "visibility": "hidden" });// css({ "visibility": "none" });
            var intCols = $("#intCols")[0].selectize;
            intCols.clear();
            var cols = $("#cols")[0].selectize;
            cols.clear();
        } else {
            $("#file1_progress").css({ "height": "auto" });
            $("#file1_progress .progress-bar").css({ "visibility": "visible" });
        }
    });
    $("#submit").on("click",function(){
      $("#collapseOne").collapse("show");
        Shiny.onInputChange('flg', 0);
        $("#txtFilter").val("");
        Shiny.onInputChange('txtFilter',"");
        $("#wcContainer").slideDown("slow");
         $("#selWord")[0].innerText="";
        $("#wc2Container").css({"display":"none"});
        d3.select("#plot2 svg").remove();
        $("#tbl").css({"display":"none"});
        $("#corPlot").css({"display":"none"});
    })
    $(document).on('DOMNodeInserted', 'svg.rWordCloud', function (e) {
        //if ($(e.target).is('svg')) {
        $(".rWordCloud").addClass("hasmenu");
        //}
    });

    $("#plot").addClass("hasmenu").css({ margin: "0px auto" });
    $("#plot1").addClass("hasmenu").css({ margin: "0px auto" });
    $("#plot2").addClass("hasmenu").css({ margin: "0px auto" });
    $("#d3cluster").addClass("hasmenu");
    $("#barchart").addClass("hasmenu");
    $("#selWord").css({
        display: "inline-block",
        'font-weight': "bolder"
    });
    $("#tblWordcloud").prepend('<div class="input-group" style="margin: 10px;background-color: #374850;border-radius: 4px;border: 1px lightgray solid;width:200px;float:right;"><input id="txtFilter" type="text" class="form-control" placeholder="Search..." style="background-color: lightgray;border: 0px;color:#000"/><span class="input-group-btn"><button id="searchButton" type="button" class="btn btn-flat action-button" style="background-color: lightgray;color:#999;"><i class="fa fa-search"></i></button></span></div>');
    $("#plot").prepend('<div style=\"width:1000px\"> <button id=\"Map\" style=\"margin-right:20px;top:3em;position:relative;float:right;\"> Map  <i class=\"fa fa-share-alt\"></i></button><div style=\"width:300px;display:inline-block;\">Word Limit: <input class=\"js-range-slider\" id=\"wordLimit\" data-min=\"0\" data-max=\"100\" data-from=\"70\" data-step=\"1\" data-grid=\"true\" data-grid-num=\"10\" data-grid-snap=\"false\" data-prettify-separator=\",\" data-keyboard=\"true\" data-keyboard-step=\"1\" data-drag-interval=\"true\" data-data-type=\"number\"/ style=\"width:300px;display:inline-block\"></div></div>');
    $("#plot2").prepend('<div style=\"width:200px;display:inline-block;\">N-Gram: <input class="js-range-slider" id="ngram_2" data-min="1" data-max="3" data-from="1" data-step="1" data-grid="true" data-grid-num="2" data-grid-snap="false" data-prettify-separator="," data-keyboard="true" data-keyboard-step="50" data-drag-interval="true" data-data-type="number"/></div>')//<div style=\"width:1000px\"><div class="input-group" style="margin: 10px;background-color: #374850;border-radius: 4px;border: 1px #374850 solid;width:200px;float:right;top:2em"><input id="txtFilter" type="text" class="form-control" placeholder="Search..." style="background-color: #374850;border: 0px;color:#fff"/><span class="input-group-btn"><button id="searchButton" type="button" class="btn btn-flat action-button" style="background-color: #374850;color:#999;"><i class="fa fa-search"></i></button></span></div>
    $("#download-svg").on("click", function () {
  //    $("#download-svg").attr("onclick","tablesToExcel(['tbl1','tbl2'], ['ProductDay1','ProductDay2'], 'TestBook.xls', //'Excel')");
        window.open('data:application/vnd.ms-excel,' + encodeURIComponent($('#dvWC').html()));
    });
    $(document).on("click", "#searchButton", function (event,obj) {
     $("#wc2Container").slideDown("slow");
      
         $("#collapseTwo").collapse("show");
         if(obj === undefined){
           Shiny.onInputChange('flg', 2);
         } else {
         if(obj.src =='plot-svg'){
           Shiny.onInputChange('flg', 0);
         } else {
           if(obj.src=='ngram_2')
            Shiny.onInputChange('ngram_2dum',$("#ngram_2").val())
            
           Shiny.onInputChange('flg', 1);
         }
         }
         $("#tbl").slideDown("slow");
        $("#corPlot").slideDown("slow");
        
        Shiny.addCustomMessageHandler("testmessage",
  function(message) {
    //alert(JSON.stringify(message));
    if(message !=""){
      
    var text=JSON.stringify(message)
    var li= $('<li />').appendTo('ol.breadcrumb')
        $('<a/>', {html: text}).attr("href","#").appendTo(li);
    }
  }
);

        
    });
    $("#ngram_2").on("change",function(){
      $("#searchButton").trigger('click', {src:'ngram_2'});
    });
    
    $("#download-svg").hide();
    $("#map").on("click", function () {
        $("#submit").click();
        $(".close").click();
    });
    $('#plot').bind('DOMNodeRemoved', function () {
        $("#download-svg").hide();
    });
    $('#plot').bind("DOMNodeInserted", function () {
        $("#download-svg").show();
    });
    $("#accordion").accordion(
        { heightStyle: 'panel' }
    );

    $(document).on("click", "#plot svg text", function (event) {
        $("#collapseTwo").collapse("show");
        Shiny.onInputChange('flg', 0)
        Shiny.onInputChange("breadcrumbWord","")
        $("#wc2Container").slideDown("slow");
        $("#tbl").slideDown("slow");
        $("#corPlot").slideDown("slow");
        
        var text=d3.select(this).node().childNodes[0].data;
         var li= $('<li />').appendTo('ol.breadcrumb')
        $('<a/>', {html: text}).attr("href","#").appendTo(li);
        $("#searchButton").trigger('click', {src:'plot-svg'});
        //});
    });
    $(document).on("click", "#plot2 svg text", function (event) {
      Shiny.onInputChange('flg', 1);
      
        var text=d3.select(this).node().childNodes[0].data;
        var li= $('<li />').appendTo('ol.breadcrumb')
        $('<a/>', {html: text}).attr("href","#").appendTo(li);
        
        $("#searchButton").click();
        
    });
$(document).on("click", ".breadcrumb li a",function(event){
  var a ="";
  var b= $(this).text();
$("ol.breadcrumb").children().each(function(i){
  
  if(a!=""){
   
   a = a+","+$(this).text()
  }
  else{
    a = $(this).text();
   
  }
   if($(this).text()==b){return false;}
})
Shiny.onInputChange("breadcrumbWord",a)
        alert("You clicked on me "+a);
      $("#searchButton").trigger('click', {src:'breadcrumb'});
        
    });
    $("#accordion1").on('show.bs.collapse', function () {
        $('#accordion1 .in').collapse('hide');
    });
    $(document).contextmenu({
        delegate: ".hasmenu",
        autoFocus: true,
        preventContextMenuForPopup: true,
        preventSelect: true,
        taphold: true,
        menu: [
        //	{ title: "Cut <kbd>Ctrl+X</kbd>", cmd: "cut", uiIcon: "ui-icon-scissors" },
        //	{ title: "Copy <kbd>Ctrl+C</kbd>", cmd: "copy", uiIcon: "ui-icon-copy" },
        //	{ title: "Paste <kbd>Ctrl+V</kbd>", cmd: "paste", uiIcon: "ui-icon-clipboard", disabled: true },
			{title: "Download <kbd>Ctrl+D</kbd>", cmd: "download", uiIcon: "ui-icon-download" }
        //	{ title: "----" },
        //	{ title: "More", children: [
        //		{ title: "Sub 1 (callback)", action: function (event, ui) { alert("action callback sub1"); } },
        //		{ title: "Edit <kbd>[F2]</kbd>", cmd: "sub2", tooltip: "Edit the title" },
        //		]
        //	}
			],
        // Handle menu selection to implement a fake-clipboard
        select: function (event, ui) {
            var $target = ui.target;
            switch (ui.cmd) {

                case "download":
                    if ($target[0].localName == "svg" || $target[0].localName == "text") {
                      // var svgText = d3.select($target[0]).attr("version", "1.1").attr("xmlns", "http://www.w3.org/2000/svg").node().outerHTML; //.parentNode.innerHTML;
//if (svgText===undefined)
//{
  var outer = document.createElement('div');
  outer.appendChild(($target.attr("version", "1.1").attr("xmlns", "http://www.w3.org/2000/svg"))[0].cloneNode(true));
  var svgText= outer.innerHTML;
  
//}
//saveSvgAsPng($target[0], "diagram.png");
var myCanvas = document.createElement('canvas');
                        var myCanvasContext = myCanvas.getContext('2d');
                        myCanvas.width = ui.target.width();//800;
                        myCanvas.height = ui.target.height();//600;
                        // Load up our image.
                        var source = new Image();
                        source.width = ui.target.width();//800;
                        source.height = ui.target.height();//600;
                        var svgURL = new Blob([svgText], { type: "image/svg+xml;charset=utf-8" }),
                        domURL = self.URL || self.webkitURL || self;
                        source.src = domURL.createObjectURL(svgURL);

                        // Render our SVG image to the canvas once it loads.
                        source.onload = function () {
                            myCanvasContext.drawImage(source, 0, 0);
                            var a = document.createElement("a");
                            //var a = document.getElementById('link');
                            a.download = "wordcloud"+ui.target.parent()[0].id+".png";
                            a.href = myCanvas.toDataURL('image/png');

                            //document.body.appendChild(a);
                            a.click();
                            $("#imgWC"+ui.target.parent()[0].id).attr("src", a.download);
                            //"C:\\Users\\saravanan_raju\\Downloads\\"+a.download);//myCanvas.toDataURL('image/png'));
                            //window.open('data:application/vnd.ms-excel,' + encodeURIComponent($('#dvWC').html()));

                        };



                    } else {
                        var a = document.getElementById('link');
                        var txt = $(".menu").text().trim();
                        var res = txt.split(" ");
                        a.download = res[res.length - 1] + " correlation.png";
                        a.href = $("#plot1 img")[0].src;

                        a.click();
                    }
                    break;
            }

            // Optionally return false, to prevent closing the menu now
        },
        // Implement the beforeOpen callback to dynamically change the entries
        beforeOpen: function (event, ui) {
            //      var $menu = ui.menu,
            //$target = ui.target,
            //extraData = ui.extraData; // passed when menu was opened by call to open()

            // console.log("beforeOpen", event, ui, event.originalEvent.type);

            //$("span .ui-icon").addClass("fa fa-download").removeClass("ui-icon").style({ "position": "absolute", "left": ".5em", "top": ".5em" });

            // Optionally return false, to prevent opening the menu now
        }
    });
$('#accordion1').on('hidden.bs.collapse', toggleChevron);
$('#accordion1').on('shown.bs.collapse', toggleChevron);
});


function toggleChevron(e) {
    $(e.target)
        .prev('.panel-heading')
        .find("i.indicator")
        .toggleClass('glyphicon-plus glyphicon-minus');
}
