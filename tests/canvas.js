function callColor (canvas, color) {
    var ctx = canvas[0].getContext("2d");
    ctx.fillStyle='#FF0000';
}

function callMoveTo (canvas, x, y) {
    var ctx = canvas[0].getContext("2d");
    ctx.moveTo(x,y);
}

function callLineTo (canvas, x, y) {
    var ctx = canvas[0].getContext("2d");
    ctx.lineTo(x,y);
}

function callBeginPath (canvas) {
    var ctx = canvas[0].getContext("2d");
    ctx.beginPath();
}

function callClosePath (canvas) {
    var ctx = canvas[0].getContext("2d");
    ctx.closePath();
}

function callStroke (canvas) {
    var ctx = canvas[0].getContext("2d");
    ctx.stroke();
}

