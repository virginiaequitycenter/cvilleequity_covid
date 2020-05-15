// Three function that change the tooltip when user hover / move / leave a cell
var mouseover = function(d) {
    
    d3.select("#tooltip")
      .style("visibility","visible");
    
    d3.select(this)
      .style("stroke", "black")
      .style("opacity", 1)
    
    
  var id = d3.select(this).attr("id"); 
      id = id.substring(1, id.length);
  
        d3.select("#countyname").text(d.CountyName + " " )
        d3.select("#tractname").text(d.NAMELSAD  )

    d3.select("#indicatorname").text( d.Label + ": ")
  d3.select("#indicatorvalue").text( d.Percent)

    d3.select("#value").text("Normalized Index: " +  d.Number)

/// Dim all    
d3.selectAll(".tractshapes").transition().style("opacity", .2)
/// Highlight the Row    
var highlightid = "#T" + id;   
d3.selectAll(highlightid).transition().style("opacity", 1)    
     
  }


var mouseoverbar = function(d) {
    
    d3.select("#tooltip")
      .style("visibility","visible");
    
    d3.select(this)
      .style("stroke", "black")
      .style("opacity", 1)
    
    
  var id = d3.select(this).attr("id"); 
      id = id.substring(1, id.length);
  
        d3.select("#countyname").text(d.CountyName + " " )
        d3.select("#tractname").text(d.NAMELSAD  )

    d3.select("#indicatorname").text( d.Label + ": ")
  d3.select("#indicatorvalue").text( d.Number)

    d3.select("#value").text("")

/// Dim all    
d3.selectAll(".tractshapes").transition().style("opacity", .2)
/// Highlight the Row    
var highlightid = "#T" + id;   
d3.selectAll(highlightid).transition().style("opacity", 1)    
     
  }





var mousemove = function(d) {
    
    d3.select("#tooltip")
      .style("left", d3.event.pageX -100  + "px")
      .style("top", d3.event.pageY -100 + "px")

  }
  

  var mouseleave = function(d) {
  d3.select("#tooltip")
      .style("visibility","hidden");
      
    d3.select(this)
      .style("stroke", "none")
      .style("opacity", 0.8)
      
  d3.selectAll(".tractshapes").transition().style("opacity", .8)

  }
  
  
  
  
  

// Three function that change the tooltip when user hover / move / leave a cell

var mouseovermap = function(d) {
    
    
    d3.select("#tooltip")
      .style("visibility","visible");
    
    d3.select(this)
      .style("stroke", "black");
    
var id = d3.select(this).attr("id"); 
    
     id = id.substring(1, id.length);
        
var tooltip_data = bar_data.filter(function(el) {
         return el.GEOID === id ;})

    
function modify_tooltip(d)    {    
        d3.select("#countyname").text(d.CountyName + " " )
        d3.select("#tractname").text(d.NAMELSAD  )

    d3.select("#indicatorname").text( d.Label + ": ")
  d3.select("#indicatorvalue").text( d.Number)

    d3.select("#value").text("")
 }
    
modify_tooltip(tooltip_data[[0]]);

/// Dim all    
d3.selectAll(".rectangle").transition().style("opacity", .2)
/// Highlight the Row    
var highlightid = "#A" + id;   
d3.selectAll(highlightid).transition().style("opacity", 1)    
    
    
}


var mousemovemap = function(d) {
    
    d3.select("#tooltip")
      .style("left", d3.event.pageX -100  + "px")
      .style("top", d3.event.pageY -100 + "px")
      

  }
  
  var mouseleavemap = function(d) {
  d3.select("#tooltip")
      .style("visibility","hidden");
      
    d3.select(this)
      .style("stroke", "white")
      
      d3.selectAll(".rectangle").transition().style("opacity", .8)

  }
  
  
var mouseclick = function(d) {
 var id = d3.select(this).attr("id"); 
     id = id.substring(1, id.length);
    
maketable(tract_facts, id); 
}

function showmethods(){
   d3.selectAll("#methods").classed("closed", false) ;
    
}

function closemethods(){
   d3.selectAll("#methods").classed("closed", true) ;
    
}