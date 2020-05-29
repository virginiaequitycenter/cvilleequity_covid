var mycolumns;
var myrows;
var table_data;
var table_nested;

function  maketable(input_data, tract) {

table_data = input_data.filter(function(el) {
      return el.GEOID === tract; });

table_nested = d3.nest()
            .key(function(d){ return  d.Label})
           .entries(table_data)

    
   mycolumns = d3.map(table_data, function(d){return d.Stat;}).keys();
   var statistic = ["Statistic"]
   mycolumns = statistic.concat( mycolumns);
   myrows = d3.map(table_data, function(d){return d.Label;}).keys()

    var  tablebox = d3.selectAll(".tablecontent")
      tablebox.selectAll("table").remove()
    var table = tablebox.append("table").classed("table", true).classed("table-sm", false).attr("id", "tractfacts")


var tablehead = table.append("thead")
      .attr("class", "thead-light")
     .append("tr")
     .attr("class", "tablehead")
    
    
tablehead.selectAll("th")
       .data(mycolumns)
     .enter()
     .append("th")
     .attr("scope", "col")
     .text(function (d) {return d})
    
    
var tablebody = table.append("tbody")

var tablerows = tablebody.selectAll('tr')
         .data(table_nested)
         .enter()
         .append("tr")
         .attr("class", "table-hover")


var rowlabels = tablerows.append("th")
          .attr("scope", "row")
          .style("class", "rowtitles")
          .text(function(d) {return d.key})
    
    
var cellcontent = tablerows.selectAll("td")
         .data(function(d){return d.values.slice()})
         .enter()
         .append("td")
         .text(function(d) {return d.Value})
    

    
var header = d3.selectAll("#tableheader")
    


var tableheader = table_data[[0]]["NAME"]    
header.selectAll("h3")
      .text(function() {return  tableheader })
    
    
         //.text( function() {return  table_data[[0]]["CountyName"] })
         
         
};


