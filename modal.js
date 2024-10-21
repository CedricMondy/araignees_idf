// Ouvre la modale
function openModal(imageSrc) {
  document.getElementById('modalImg').src = imageSrc;
  document.getElementById('myModal').style.display = "block";
}

// Ferme la modale
function closeModal() {
  document.getElementById('myModal').style.display = "none";
}

// Ferme la modale en cliquant en dehors de l'image
window.onclick = function(event) {
  var modal = document.getElementById('myModal');
  if (event.target === modal) {
    closeModal();
  }
}
