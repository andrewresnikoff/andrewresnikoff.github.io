var __PROTECT__ = true

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

function readTextFile(file)
{
    var rawFile = new XMLHttpRequest();
    rawFile.open("GET", file, false);
    rawFile.onreadystatechange = function ()
    {
        if(rawFile.readyState === 4)
        {
            if(rawFile.status === 200 || rawFile.status == 0)
            {
                var allText = rawFile.responseText;
                alert(allText);
            }
        }
    }
    rawFile.send(null);
}

$(".protect").click(function() {
	if (__PROTECT__){
		attempt = prompt("This document is password protected. Please enter the password to continue.");
		if (attempt){
			hash = attempt.hashCode();
  			pass = readTextFile("_hash.txt");
  			if (hash == pass){
  				break;
  			}
		}
	}
	else{
		return false;
	}
});