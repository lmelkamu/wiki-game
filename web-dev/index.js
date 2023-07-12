function changeyes() {
    document.getElementById("no").innerHTML = "Wrong Answer >:(";
}

function correct() {
    document.getElementById("yes").innerHTML = "YAY"
    document.getElementById("ballons").style.display = "block";
}