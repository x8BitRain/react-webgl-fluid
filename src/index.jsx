import React from 'react';
import ReactDOM from 'react-dom';
import Fluid from './components/fluid.jsx';
import '../assets/stylesheets/application.scss';

const root = document.getElementById('root');
if (root) {
  ReactDOM.render(<Fluid />, root);
}
