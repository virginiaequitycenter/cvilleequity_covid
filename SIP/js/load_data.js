var burden_data;
var tract_facts;
var attributes;

function loadData() {

initiateLeaflet();
    
    
d3.csv("data/burden_data.csv")
    .then(
      
        function(data) {   
            burden_data = data;
            loadVis(burden_data, "norm");
            

        });  
    
 d3.csv("data/tract_facts.csv")
    .then(
      function(data) {
          tract_facts = data;
          maketable(tract_facts, "51003010700");
      });
     
    
//    d3.csv("data/attributes.csv")
//        .then(
//      function(data) {
//          attributes = data;
//          attributetable(attributes);
//      });
     
}


var bar_data;
var heatmap_data;
var all_data;

function loadVis(input_data, metric){
    
   all_data = input_data.filter(function(el) {
    return el.Index === metric })
        
   heatmap_data = input_data.filter(function(el) {
    return el.Index === metric && el.Domain !== "index";})

    bar_data = input_data.filter(function(el) {
      return el.Index === metric && el.Domain === "index";})
    
    heat_bars(input_data, heatmap_data, bar_data);     
    
    colorLeaflet(all_data, "Composite Score");
}
