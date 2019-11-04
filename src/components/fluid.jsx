import React from 'react';

export default class Fluid extends React.Component {
    constructor(props) {
    super(props);
  }


  componentDidMount() {
    const script = document.createElement("script");
    script.src = "./assets/js/fluid-init.js";
    script.async = true;
    script.onload = () => this.scriptLoaded();

    document.body.appendChild(script);
  }

  scriptLoaded() {
    console.log("WebGL Canvas Loaded // also a callback function if you need it.")
  }

  render() {
    return (
      <canvas className="fluid-canvas"></canvas>
    );
  }
}





