var force = null;
var vis = null;
var editor = null;

var w = 1280,
    h = 800,
    node,
    nodes,
    links,
    link,
    root;


var currentNodesToUnfold = [root];
var elapsedTimer = 2000;
var isFirst = true;
var isFirst = true;

$(document).ready(function() {
    editor = ace.edit("editor");
    editor.setTheme("ace/theme/twilight");
    editor.getSession().setMode("ace/mode/c_cpp");

    force = d3.layout.force()
        .on("tick", tick)
        .charge(function(d) { return d.charge; })
        .linkDistance(function(d) { return d.source.linkDistance; })
        .linkStrength(function(d) {
          return 1.0;
        })
        //.chargeDistance(400)
        //.gravity(0.2)
        .friction(0.4)
        .size([w, h - 160]);

    vis = d3.select("#astView").append("svg")
                .attr("width", w)
                .attr("height", h);
});

function initNode(node) {
    node.isCollapsed = true
    node.charge = 0
    node.chargeChange = 0.0
    node.linkDistanceChange = 0.0
    node.linkDistance = 100.0
    node.linkStrength = 0.0
    node.x = w / 2;
    node.y = h / 2 - 80;
    node.isClicked = false;
    node.growthSpeed = Math.floor((Math.random() * 4) + 1);
}

// Color leaf nodes orange, and packages white or blue.
function color(d) {
  if (d == root) {
    return "#ff0000"
  } else if (d.type == "CASTBinaryExpression") {
	return "#00FF00"
  } else if (d._children.length > 0) {
    return "#3182bd";
  } else if (d.children.length > 0) {
    return "#c6dbef";
  } else {
    return "#fd8d3c";
  }
}

function openNode(d) {
  d.linkDistance = 0.0
  d.isCollapsed = false
  var node = d._children[0]
  node.isCollapsed = true // children start out closed
  node.x = d.x;
  node.y = d.y;
  node.px = d.x;
  node.py = d.y;
  node.charge = 0;
  node.size = 0.0;
  node.linkStrength = 0.0;
  node.chargeChange = 0.0;
  node.linkDistance = 0.0;
  node.needsToGrow = true;

  d.children.push(d._children.shift());
}

// Toggle expansion on right-click.
function contextMenu(d) {
  openNode(d);
  update();
}

function unfold(node) {
  node.isCollapsed = false;

  while (node._children.length > 0) {
    openNode(node);
  }
}



  // Returns a list of all nodes under the root.
function flatten(root) {
      var nodes = [], i = 0;

      function recurse(node) {
        if (node.children) node.numChildren = node.children.reduce(function(p, v) { return p + recurse(v); }, 0);
        if (!node.id) node.id = ++i;
        nodes.push(node);
        return node.numChildren;
      }

      root.numChildren = recurse(root);
      return nodes;
}

function highlightNode(node) {
    var i = 0;
    var currentOffset = 0
    var keepLooping = node.offset != 0
    var offset = 0

    function addMarker(row, lineText, currentOffset, desiredOffset, length) {
      var Range = ace.require("ace/range").Range

      if (desiredOffset + length > currentOffset + lineText.length - 1) {
          // if the current node contains this whole line, highlight it all except for spaces
          var j = 0;
          var startCharIndex = -1;
          var lastCharIndex = -1;
          for (j = 0; j < lineText.length; j++) {
             if (lineText[j] != ' ' && startCharIndex == -1) {
               startCharIndex = j;
               lastCharIndex = j + 1;
             } else if (lineText[j] != ' ') {
               lastCharIndex = j + 1;
             }
          }

          editor.getSession().addMarker(new Range(row, startCharIndex, row, lastCharIndex),'errorHighlight');
      } else {
         // the current node completely resides on this line
         var column = desiredOffset - currentOffset
         editor.getSession().addMarker(new Range(row, column, row, column + length),'errorHighlight');
      }
    }

    while (keepLooping) {
       var lineText = editor.session.getLine(i);

       if (currentOffset + lineText.length + 1 >= node.offset + node.length) {
          keepLooping = false;
          addMarker(i, lineText, currentOffset, node.offset, node.length);
       } else if (currentOffset + lineText.length + 1 >= node.offset) {
          addMarker(i, lineText, currentOffset, node.offset, node.length);
          currentOffset += lineText.length + 1
          i += 1;
       } else {
          currentOffset += lineText.length + 1
          i += 1;
       }
    }
  }

function tick() {

      if (isFirst) {
        unfold(root);
        isFirst = false;
      } else {
        update();
      }

      link.attr("x1", function(d) { return d.source.x; })
          .attr("y1", function(d) { return d.source.y; })
          .attr("x2", function(d) { return d.target.x; })
          .attr("y2", function(d) { return d.target.y; });

      if (nodes != null) {
        nodes.forEach(function(node) {

          if (!node.isCollapsed) {
             if (node.charge > -300.0) {
               node.charge -= node.chargeChange;
               node.chargeChange += 0.3
             }
             if (node.linkDistance < 20.0) {
               node.linkDistance += node.linkDistanceChange * node.growthSpeed;
               node.linkDistanceChange += 0.003

               if (node.linkDistance >= 20.0) {
                  var i = 200;

                  // expand the children now
                  node.children.forEach(function(x) {
                    d3.timer(function(elapsed) {
                       unfold(x);
                       return 1;
                    }, i);
                    i += 400;
                  });
               }
             }
          }
        });
      }

      node.on('mouseover', function(d) {

        d3.select(this).select("circle").transition()
              .duration(350)
              .style("r", 10.0)
              .style('stroke-width', 3);

           link.transition()
              .duration(350).style('stroke-width', function(l) {
                if (d === l.source || d === l.target)
                  return 4;
                else
                  return 2;
              });
      });

      node.on("click", function (node) {
          var clickedCircles = vis.selectAll("circle.node").filter(function(i) {
            return i.isClicked;
          });

          var clicked = flatten(root).filter(function(i) {
            return i.isClicked;
          });

          node.isClicked = true;

          console.log(node.type);

          var markers = editor.getSession().getMarkers(false)

          $.each(markers, function(marker) {
              editor.getSession().removeMarker(marker);
          });

          highlightNode(node);

          d3.select(this).select("circle").transition()
                .duration(350)
                .style('stroke', '#FF0000')
                .style('stroke-width', 4.0);

          // unclick any other clicked nodes
          clickedCircles.transition()
              .duration(350)
              .style("r", 6.0)
              .style('stroke-width', 3.0)
              .style('stroke', '#000000');

          $.each(clicked, function(d) {
              d.isClicked = false;
          });
      });

      node.on("mouseout", function (d) {
        if (!d.isClicked) {
          link.transition()
          .duration(350)
          .style('stroke-width', 2.0);

          d3.select(this).select("circle").transition()
          .duration(350)
          .style("r", 6.0)
          .style('stroke-width', 3.0)
          .style('stroke', '#000000');
        } else {
          d3.select(this).select("circle").transition()
          .duration(350)
          .style("r", 6.0)
          .style('stroke', '#FF0000')
          .style('stroke-width', 3.0);
        }
      });

      d3.selectAll("circle").attr("cx", function (d) {
            return d.x;
        })
            .attr("cy", function (d) {
            return d.y;
      });

      d3.selectAll("text").attr("x", function (d) {
          return d.x;
        })
          .attr("y", function (d) {
            return d.y;
      });
    }


function update() {

     nodes = flatten(root),
      links = d3.layout.tree().links(nodes);

      // Restart the force layout.
      force
          .nodes(nodes)
          .links(links)
          .start();

  // Update the links…
  link = vis.selectAll("line.link")
      .data(links, function(d) { return d.target.id; });

  // Enter any new links.
  link.enter().insert("svg:line", ".node")
      .attr("class", "link")
      .style('stroke-width', 4.0)
      .style('stroke', '#AA0000')
      .attr("x1", function(d) { return d.source.x; })
      .attr("y1", function(d) { return d.source.y; })
      .attr("x2", function(d) { return d.target.x; })
      .attr("y2", function(d) { return d.target.y; })
      .transition()
      .duration(1000)
      .style('stroke', '#000000')
      .style('stroke-width', 2.0)

  // Exit any old links.
  link.exit().remove();

  var newNodes = vis.selectAll("circle.node")
      .data(nodes, function(d) { return d.id; });

  // Update the nodes…
  node = newNodes.enter().append("g")
      .attr("class", "node")
      .call(force.drag);

  // Enter any new nodes.
  node.append("circle")
      .attr("class", "node")
      .style('r', 0.0)
      .attr("cx", function(d) { return d.x; })
      .attr("cy", function(d) { return d.y; })
      .style('stroke-width', 3)
      .property("isClicked", false)
      .style("fill", color)
      .on("contextmenu", contextMenu)
      .call(force.drag)
      .transition()
      .duration(1000)
      .style('r', 6.0);

  node.append("text")
      .attr("dx", 10)
      .attr("dy", ".35em")
      .style("opacity", 0.0)
      .text(function(d) { return d.name; })
      .transition()
      .duration(350)
      .style('opacity', 1.0);

  // Exit any old nodes.
  newNodes.exit().remove();

}

function submitCode() {
    $.getJSON("getAst",
        {
          code: editor.getValue(),
          width: 640,
          height: 480
        },
        function( data ) {
            var json = eval(data);

         // alert(JSON.stringify(json));

          root = json;
          root.fixed = true;
          root.x = w / 2;
          root.y = h / 2 - 80;

          flatten(root).forEach(function(node) {

              if (node.children == null) {
                node._children = [];
                node.children = [];
              } else {
                node._children = node.children.slice();
                node.children = [];
              }
              if (node._children != null) {
                 node._children.forEach(function(x) {
                    x.parent = node;
                    initNode(x);
                 });
              }
              initNode(node);
          });

         nodes = flatten(root),
      links = d3.layout.tree().links(nodes);

      // Restart the force layout.
      force
          .nodes(nodes)
          .links(links)
          .start();

        update();
     });
}