String.prototype.hashCode = function() {
  var hash = 0, i, chr, len;
  if (this.length == 0) return hash;
  for (i = 0, len = this.length; i < len; i++) {
    chr   = this.charCodeAt(i);
    hash  = ((hash << 5) - hash) + chr;
    hash |= 0; // Convert to 32bit integer
  }
  return hash;
};

$("div").onClick{
	alert("test")
	// password = prompt("Please enter a password to view this page.");
	// alert(String.prototype.hashCode(password));
};

$( "div" ).click(function() {
  print("here\n")
  alert( "Handler for .click() called." );
});

$(".protect").click(function() {
  print("here\n")
  alert( "Handler for .click() called." );
});