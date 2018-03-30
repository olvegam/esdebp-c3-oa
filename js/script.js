function verMas() {
	document.getElementById('verMenos').style.display = 'block';
	document.getElementById('verMas').style.display = 'none';
	var completText = document.getElementsByClassName("completText");
	for (var i=0; i<completText.length; i++) {
		completText[i].style.display = 'block';
	}
}

function verMenos() {
	document.getElementById('verMas').style.display = 'block';
	document.getElementById('verMenos').style.display = 'none';
	var completText = document.getElementsByClassName("completText");
	for (var i=0; i<completText.length; i++) {
		completText[i].style.display = 'none';
	}
}