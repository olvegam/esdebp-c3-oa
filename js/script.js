function verMas(textNum) {
	document.getElementById('verMenos'+textNum).style.display = 'block';
	document.getElementById('verMas'+textNum).style.display = 'none';
	var completText = document.getElementsByClassName("completText"+textNum);
	completText[0].style.display = 'block';
	//document.getElementsByClassName("listCond").style.display = 'block';
}

function verMenos(textNum) {
	document.getElementById('verMas'+textNum).style.display = 'block';
	document.getElementById('verMenos'+textNum).style.display = 'none';
	var completText = document.getElementsByClassName("completText"+textNum);
	completText[0].style.display = 'none';
	//document.getElementsByClassName("listCond").style.display = 'none';
}
