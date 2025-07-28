async function verifyLogin() {
  const id = document.getElementById("login-id").value.trim();
  const password = document.getElementById("login-password").value.trim();

  const response = await fetch("https://script.google.com/macros/s/AKfycbxEwMZ16lWpbZXmbJal2J0MfYlexHkmu6zOs9eluXKThpMivqrGAKQk63n2kvbWF9yoJQ/exec", {
    method: "POST",
    body: JSON.stringify({ id, password }),
    headers: { "Content-Type": "application/json" }
  });

  const result = await response.json();

  if (result.status === "success") {
    document.getElementById("login-container").style.display = "none";
    document.getElementById("protected-content").style.display = "block";
  } else {
    alert("Invalid ID or password. Please try again.");
  }
}
