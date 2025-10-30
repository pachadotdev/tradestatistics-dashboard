// Add id 'waiter-content' to the main page section so Waiter overlays target it
document.addEventListener('DOMContentLoaded', function() {
  var section = document.getElementsByClassName('page');
  if (section && section.length > 0) {
    section[0].setAttribute('id', 'waiter-content');
  }
});
