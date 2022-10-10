const gUrl = "/sokoban_gui";
const succeeded = "succeeded";
const failed = "failed";
let gNonInteractiveLoop = false;
let gTimerLoop = false;


function reset() {
    makeRequest(gUrl, {"action": "reset", "need_interactive": false})
    .then(data => {
        if(data.status == succeeded) {
            clearNonInteractiveLoop();
            clearTimerLoop();
            setTimerValue(0);
            updateTiles(data);
        }
    })
    .catch(error => console.log(error));
}

function solve() {
    makeRequest(gUrl, {"action": "reset", "need_interactive": true})
    .then(data => {
            if(data.status == succeeded) {
                updateTiles(data);
                setTimerValue(0);
                createTimerLoop()();
                makeRequest(gUrl, {"action": "solve", "need_interactive": true})
                .then(data => {
                    if(data.solution && data.status == succeeded && !data.interactive)
                    {
                        playMoves(data);
                    } else if(data.status == failed)
                    {
                        clearTimerLoop();
                        alert('Could not be solved!');
                    }                   
                })
                .catch(error => console.log(error));
            } else if(!isInteractive()) {
                alert('Not in interactive mode!');
            }
        }
    )
    .catch(error => console.log(error));    
}

function boardAction(action, needInteractive) {
    makeRequest(gUrl, {"action": action, "need_interactive": needInteractive})
    .then(data => updateTiles(data))
    .catch(error => console.log(error));
}

function playMoves(data) {
    clearNonInteractiveLoop();
    clearTimerLoop();
    createPlayLoop(data)();
}

function createPlayLoop(data) {
    let moves = data.solution;
    let index = moves.length;

    return function move() {
        index--;
        if (index >= 0) {
            makeRequest(gUrl, {"action": moves[index][0], "need_interactive": false})
            .then(data => {
                if(!data.interactive) {
                    updateTiles(data);
                    let dt = 200;
                    if (index > 0 && moves[index - 1][1])
                    {
                        dt = 500;
                    }
                    gNonInteractiveLoop = setTimeout(move, dt);
                } else {
                    gNonInteractiveLoop = false;
                }
            })
            .catch(error => console.log(error));
        } else {
            makeRequest(gUrl, {"action": "interactive_on", "need_interactive": false})
            .then(data => gNonInteractiveLoop = false )
            .catch(error => console.log(error));
        }
    }
}

function formatDigits(num) {
    return num > 9 ? num.toString() : "0" + num;
}

function clearNonInteractiveLoop() {
    if(gNonInteractiveLoop) {
        clearTimeout(gNonInteractiveLoop);
        gNonInteractiveLoop = false;
    }
}

function isInteractive() {
    return !gNonInteractiveLoop;
}

function createTimerLoop() {
    // In seconds
    let elapsedTime = -1;
    clearTimerLoop();
    return function tick() {
        elapsedTime++;
        setTimerValue(elapsedTime);
        gTimerLoop = setTimeout(tick, 1000);
    }
}

function clearTimerLoop() {
    if(gTimerLoop) {
        clearTimeout(gTimerLoop);
        gTimerLoop = false;
    }
}

function setTimerValue(elapsedTime) {
    seconds = elapsedTime % 60;
    minutes = Math.floor(elapsedTime / 60) % 60;
    hours = Math.floor(elapsedTime / 3600);

    let htmlSeconds = document.getElementById("seconds");
    htmlSeconds.innerHTML = ":" + formatDigits(seconds);
    let htmlMinutes = document.getElementById("minutes");
    htmlMinutes.innerHTML = ":" + formatDigits(minutes);
    let htmlHours = document.getElementById("hours");
    htmlHours.innerHTML = hours;
}

function clearTiles() {
    let board = document.getElementById("board");
    for(let row of board.rows) {
        for(let cell of row.cells) {
            let div = cell.getElementsByTagName("div")[0];
            div.classList.remove("box", "sokoban");
        }
    }
}

function updateTiles(data) {
    if(data.status == failed) return;
    clearTiles();
    let cell = document.getElementById(data.sokoban);
    let div = cell.getElementsByTagName("div")[0];
    div.className = "sokoban";
    for(let box of data.boxes) {
        let cell = document.getElementById(box);
        let div = cell.getElementsByTagName("div")[0];
        div.className = "box";
    }
}

function makeRequest(url, content) {
        let fetchOtions = {
                method: "POST",
                mode: "cors",
                cache: "no-cache",
                headers: {
                        "Content-type": "application/json"
                },
                body: JSON.stringify(content)
        };

        return fetch(url, fetchOtions).then(
                response => response.json()
        );
}

window.onunload = function() {
    if(!isInteractive()) reset();
}

window.addEventListener("keydown", function (event) {
        if (event.defaultPrevented) {
                return;
        }

        switch (event.key) {
                case "ArrowDown":
                        boardAction("down", true);
                break;
                case "ArrowUp":
                        boardAction("up", true);
                break;
                case "ArrowLeft":
                        boardAction("left", true);
                break;
                case "ArrowRight":
                        boardAction("right", true);
                break;
                default:
                return;
        }

        event.preventDefault();
}, true);

boardAction("refresh", false);