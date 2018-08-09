import React, { Component } from 'react';
import * as PIXI from 'pixi.js';

const config = {
    uuid : "FESSE",
    playerId : 1,
    boardGraphicsSpecs : [
        {x : 10, y : 4},
        {x : 10, y : 3},
        {x : 10, y : 2},
        {x : 11, y : 2},
        {x : 12, y : 2},
        {x : 12, y : 1},
        {x : 12, y : 0},
        {x : 11, y : 0},
        {x : 10, y : 0},
        {x : 9, y : 0},
        {x : 8, y : 0},
        {x : 7, y : 0},
        {x : 6, y : 0},
        {x : 6, y : 1},
        {x : 6, y : 2},
        {x : 5, y : 2},
        {x : 4, y : 2},
        {x : 3, y : 2},
        {x : 2, y : 2},
        {x : 2, y : 1},
        {x : 2, y : 0},
        {x : 1, y : 0},
        {x : 0, y : 0},
        {x : 0, y : 1},
        {x : 0, y : 2},
        {x : 0, y : 3},
        {x : 0, y : 4},
        {x : 0, y : 5},
        {x : 0, y : 6},
        {x : 1, y : 6},
        {x : 2, y : 6},
        {x : 2, y : 7},
        {x : 2, y : 8},
        {x : 2, y : 9},
        {x : 2, y : 10},
        {x : 1, y : 10},
        {x : 0, y : 10},
        {x : 0, y : 11},
        {x : 0, y : 12},
        {x : 1, y : 12},
        {x : 2, y : 12},
        {x : 3, y : 12},
        {x : 4, y : 12},
        {x : 5, y : 12},
        {x : 6, y : 12},
        {x : 6, y : 11},
        {x : 6, y : 10},
        {x : 7, y : 10},
        {x : 8, y : 10},
        {x : 9, y : 10},
        {x : 10, y : 10},
        {x : 10, y : 11},
        {x : 10, y : 12},
        {x : 11, y : 12},
        {x : 12, y : 12},
        {x : 12, y : 11},
        {x : 12, y : 10},
        {x : 12, y : 9},
        {x : 12, y : 8},
        {x : 12, y : 7},
        {x : 12, y : 6},
        {x : 11, y : 6},
        {x : 10, y : 6},
        {x : 10, y : 5}
    ],
    targetGraphicsSpecs : [
        {x : 9, y : 4},
        {x : 9, y : 3},
        {x : 9, y : 2},
        {x : 9, y : 1},
        {x : 4, y : 3},
        {x : 3, y : 3},
        {x : 2, y : 3},
        {x : 1, y : 3},
        {x : 3, y : 8},
        {x : 3, y : 9},
        {x : 3, y : 10},
        {x : 3, y : 11},
        {x : 8, y : 9},
        {x : 9, y : 9},
        {x : 10, y : 9},
        {x : 11, y : 9}
    ]
};

const nextGameState = {
    turn : 1,
    over : false,
    cards : {
        1 : [2,5,6],
        2 : [0,0,0],
        3 : [0,0],
        4 : [0,0]
    },
    pegs : {
        1 : [{location : "Home"},{location : "Board", pos : 21, stake : false}, {location : "Target", pos : 1},{location : "Home"}],
        2 : [{location : "Home"},{location : "Board", pos : 15, stake : false}, {location : "Target", pos : 5},{location : "Home"}],
        3 : [{location : "Home"},{location : "Board", pos : 17, stake : false}, {location : "Target", pos : 10},{location : "Home"}],
        4 : [{location : "Home"},{location : "Board", pos : 16, stake : false}, {location : "Target", pos : 15},{location : "Home"}]
    },
    history : [],
    actions : []
};

class GameEngine extends Component {

    constructor(props) {
        super(props);

        this.state = {};

        this.gameState = nextGameState;
        this.nextGameState = nextGameState;
    }

    render() {
        return (
            <div>
                <h1>GameEngine</h1>
                <div ref="GameWindow">
                </div>
            </div>
        );
    }

    componentWillMount(){

    }

    componentDidMount(){
        this.app = new PIXI.Application();

        this.refs.GameWindow.appendChild(this.app.view);
        PIXI.loader.load((loader, resources) => {
            let board = new PIXI.Container();
            board.setTransform(400,300);

            for(let i in config.boardGraphicsSpecs){
                let e = config.boardGraphicsSpecs[i];
                let graphics = new PIXI.Graphics();

                graphics.beginFill(0xCCCC00);

                graphics.lineStyle(4, (i%4 === 0)?((i%16 === 0)?0xFF0000:0xAA0000):0x110000);

                graphics.drawRect(e.y * 40 - 256, e.x * 40 - 256, 36, 36);
                board.addChild(graphics);
            }

            for(let e of config.targetGraphicsSpecs){
                let graphics = new PIXI.Graphics();

                graphics.beginFill(0xCCCC00);

                graphics.lineStyle(4, 0xAAAAFF);

                graphics.drawRect(e.y * 40 - 256, e.x * 40 - 256, 36, 36);
                board.addChild(graphics);
            }


            this.pegsGraphicsArray = {};
            for(let i in this.gameState.pegs){
                this.pegsGraphicsArray[i] = {};
                for(let j in this.gameState.pegs[i]){
                    let peg = new PIXI.Container();

                    let graphics = new PIXI.Graphics();

                    const color =   (i === '1')?  0x00FF00:
                                    (i === '2')?  0xFF0000:
                                    (i === '3')?  0x0000FF:
                                                0xFFFF00;
                    console.log(i,color);
                    graphics.beginFill(color);
                    graphics.lineStyle(1, 0x000000);

                    graphics.drawRect(-260,-260,20,20);

                    peg.addChild(graphics);

                    this.pegsGraphicsArray[i][j] = peg;

                    board.addChild(this.pegsGraphicsArray[i][j]);
                }
            }

            console.log(this.pegsGraphicsArray);
            this.app.stage.addChild(board);
            this.app.ticker.add(() => {
                this.gameState = this.nextGameState;

                for(let i in this.gameState.pegs){
                    for(let j in this.gameState.pegs[i]){
                        if(this.gameState.pegs[i][j].location === "Board" && this.gameState.pegs[i][j].pos) {
                            const x = config.boardGraphicsSpecs[this.gameState.pegs[i][j].pos].x,
                                  y = config.boardGraphicsSpecs[this.gameState.pegs[i][j].pos].y;
                            console.log(x,y);
                            this.pegsGraphicsArray[i][j].setTransform(y*40+10,x*40+10);
                        }
                        else if(this.gameState.pegs[i][j].location === "Target" && this.gameState.pegs[i][j].pos){
                            const x = config.targetGraphicsSpecs[this.gameState.pegs[i][j].pos].x,
                                y = config.targetGraphicsSpecs[this.gameState.pegs[i][j].pos].y;
                            this.pegsGraphicsArray[i][j].setTransform(y*40+10,x*40+10);
                        }
                        else
                        {
                            const x = j * 0.2 + (
                                                (i === "1")? 11:
                                                (i === "2")? 3 :
                                                (i === "3")? 0 :
                                                7),
                                  y = j * 0.2 + (
                                                (i === "1")? 3:
                                                (i === "2")? 0 :
                                                (i === "3")? 7 :
                                                11);
                            console.log(x,y);
                            this.pegsGraphicsArray[i][j].setTransform(y*40+10,x*40+10);
                        }
                    }
                }

            });
        });
    }
}

export default GameEngine;