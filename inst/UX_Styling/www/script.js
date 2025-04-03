document.addEventListener("DOMContentLoaded", function () {
  console.log("Javascript loaded!");

  const nextButton = document.getElementById("next_page");

  if (!nextButton) {
    console.log("Button with ID 'next_page' not found.");
    return;
  }

  function updateButtonText() {

    let buttonText = nextButton.innerText.trim().toLowerCase(); // normalise text
    console.log("Current button text:", buttonText);

    if (buttonText.includes("dashboard")) {
      nextButton.classList.add("create-dashboard");
      console.log("class added");
    } else {
      nextButton.classList.remove("create-dashboard");
      console.log("class removed");
    }
  }

  updateButtonText();

  const observer = new MutationObserver(updateButtonText);
  observer.observe(nextButton, { childList: true, subtree: true });

  console.log("MutationObserver is watching for text changes...");
})


document.addEventListener("DOMContentLoaded", function() {

  console.log("DOM fully loaded and parsed");

  let elements = document.querySelectorAll(".progress-bar");
  console.log(`Found ${elements.length} elements with class 'progress-bar'`);

  elements.forEach((element, index) => {
    console.log(`Element ${index} initial text:`, [...element.textContent]);

    if (element.textContent.includes("Upload complete")) {
      console.log(`Updating text for element ${index}`);
      element.textContent = element.textContent.replace("Upload complete", "Load complete");
    }

    let textobserver = new MutationObserver(mutations => {
      mutations.forEach(mutation => {
        if(mutation.type === "childList" || mutation.type === "characterData") {
          let newText = element.textContent;
          console.log(`Detected change in element ${index}:`, [...newText]);

          if(newText.includes("Upload complete")) {
            console.log(`Updating text for element ${index} after change`);
            element.textContent = newText.replace("Upload complete", "Load complete");
          }
        }
      });
    });

    textobserver.observe(element, { childList: true, characterData: true, subtree: true });

  });

  console.log("Script execution finished");

});
