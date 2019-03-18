<div align="center">
  <img src="tl.png">
</div>

# Theorem Livecoding

A live coding environment for theorem proving. This software consists of three components:

- Theory: Set of axioms and theorems.
- Visualizer: A program which generates GLSL code from Theory.
- Application: A livecoding environment for Theory and Visualizer. It also renders output of Visualizer to the display.

The application architecture is described in the figure below.

![](architecture.png)

Both Theory and Visualizer are written in Haskell.