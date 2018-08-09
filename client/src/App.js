import React, { Component } from 'react';
import './App.css';

//Component import
import GameEngine from './component/GameEngine';

class App extends Component {
  render() {
    return (
      <div className="App">
        <div className="App-body">
            <GameEngine/>
        </div>
      </div>
    );
  }
}

export default App;
