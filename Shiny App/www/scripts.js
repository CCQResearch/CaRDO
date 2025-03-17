// Adds and removes a specific class to the body element
// Targets that class to alter the bg color for a specific panel (Methods)

console.log("scripts.js loaded successfully");

$(document).on('shown.bs.tab', function(e) {

  var activePane = $('.tab-pane.active');
  var panelValue = activePane.attr('data-value');
  
  if (panelValue === 'Methods') {
    $('body').addClass('methods-active');
    $('').addClass('fade-in');
  } else {
    $('body').removeClass('methods-active');
    $('').removeClass('fade-in');
  }
  
});

// Hover tooltips on lifetime risk

$(document).on("shiny:value", function (event) {
  
  if (event.target.id.includes("ltr_vis")) {
    console.log(`LTR visualisation updated for ${event.target.id}, adding event listeners`);
  
    setTimeout(function() {
      
      let circles = document.querySelectorAll("#" + event.target.id + " .data-circle");
      
      if (circles.length > 0) {
        console.log(`Circles found: ${circles.length} in ${event.target.id}`);
        
        circles.forEach(circle => {
          
          let tooltip = document.createElement("div");
          tooltip.classList.add("ltr-tooltip");
          tooltip.style.position = "absolute";
          tooltip.style.zIndex = "9999";
          tooltip.style.backgroundColor = "rgba(0, 0, 0, 0.5)";
          tooltip.style.color = "white";
          tooltip.style.padding = "5px 10px";
          tooltip.style.borderRadius = "5px";
          tooltip.style.display = "none";
          tooltip.style.pointerEvents = "none";
          tooltip.style.fontSize = "0.8rem";
          
          document.body.appendChild(tooltip);
          
          circle.addEventListener("mouseover", function(event) {
            tooltip.innerText = circle.getAttribute("data-info");
            tooltip.style.display = "block";
            tooltip.style.left = (event.pageX + 10) + "px";
            tooltip.style.top = (event.pageY + 10) + "px";
          });
          
          circle.addEventListener("mousemove", function(event) {
            tooltip.style.left = (event.pageX + 10) + "px";
            tooltip.style.top = (event.pageY + 10) + "px";
          });
          
          circle.addEventListener("mouseout", function(event) {
            tooltip.style.display = "none";
          });
          
        });
      } else {
        console.warn(`No circles found! ${event.target.id}`);
      }
    }, 500);
  }
});