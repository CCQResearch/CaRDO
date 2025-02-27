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
