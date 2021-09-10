<!--
*** Based on the Best-README-Template: https://github.com/othneildrew/Best-README-Template
***
*** To avoid retyping too much info. Do a search and replace for the following:
*** repo_name, project_title, project_description
-->



<!-- PROJECT SHIELDS -->
<!-- [![Release][release-shield]][release-url] -->
<!-- [![Last Commit][last-commit-shield]][last-commit-url] -->
<!-- [![Contributors][contributors-shield]][contributors-url] -->
<!-- [![Forks][forks-shield]][forks-url] -->
<!-- [![Stargazers][stars-shield]][stars-url] -->
<!-- [![Issues][issues-shield]][issues-url] -->
<!-- [![MIT License][license-shield]][license-url] -->
<!-- [![LinkedIn][linkedin-shield]][linkedin-url] -->



<!-- PROJECT LOGO -->
<br />
<p align="center">
<!--   <a href="https://github.com/Tim-W-James/repo_name">
    <img src="images/logo.png" alt="Logo" width="80" height="80">
  </a> -->

  <h2 align="center">Sushi Go AI</h2>

  <p align="center">
    Artificial intelligence algorithm that can determine optimal moves to take in a game of Sushi Go.
    <br />
    This project was created during my university studies at <b>ANU</b> in <b>2019</b> and has been transferred from the ANU GitLab server.
    <br />
    <a href="https://github.com/Tim-W-James/Sushi-Go-AI/blob/master/report.pdf"><strong>Read the technical report »</strong></a>
    <br />
    <br />
<!--     <a href="https://github.com/Tim-W-James/repo_name">View Demo</a> -->
<!--     ·
    <a href="https://github.com/Tim-W-James/repo_name/issues">Report Bug</a> -->
<!--     ·
    <a href="https://github.com/Tim-W-James/repo_name/issues">Request Feature</a> -->
  </p>
</p>



<!-- TABLE OF CONTENTS -->
<details open="open">
  <summary>Table of Contents</summary>
  <ol>
    <li>
      <a href="#about-the-project">About The Project</a>
      <ul>
        <li><a href="#built-with">Built With</a></li>
      </ul>
    </li>
    <li>
      <a href="#usage">Usage</a>
      <ul>
        <li><a href="#prerequisites">Prerequisites</a></li>
        <li><a href="#installation">Installation</a></li>
        <li><a href="#how-to-play">How to play</a></li>
      </ul>
    </li>
    <li><a href="#contact">Contact</a></li>
    <li><a href="#acknowledgements">Acknowledgements</a></li>
  </ol>
</details>



<!-- ABOUT THE PROJECT -->
## About The Project

[![Product Name Screen Shot][product-screenshot]](https://example.com)

Using a recursive minimax algorithm and the functional programming language [Haskell](https://www.haskell.org/platform/) I created an AI that will play [Sushi Go](https://www.ultraboardgames.com/sushi-go/game-rules.php).
Find the core algorithm in [src/AI.hs](https://github.com/Tim-W-James/Sushi-Go-AI/blob/master/src/AI.hs).
To find out how the algorithm has been implemented, read the [report](https://github.com/Tim-W-James/Sushi-Go-AI/blob/master/report.pdf).

### Built With

* [Haskell Functional Programming Language](https://www.haskell.org/platform/)



<!-- USAGE -->
## Usage

### Prerequisites

* To run requires the Global Haskell Compiler and Cabal: https://www.haskell.org/platform/
* To develop use [IntelliJ IDEA](https://www.jetbrains.com/idea/)

### Installation

1. Clone the repo
   ```sh
   git clone https://github.com/Tim-W-James/Sushi-Go-AI.git
   ```
2. Navigate into the root directory and run
   ```sh
   cabal build
   cabal run sushigo
   ```

### How to play
* Sushi Go Rules (Sushi Go AI uses open hands): https://www.ultraboardgames.com/sushi-go/game-rules.php
* The hands and cards in play for each player are printed to the terminal
* Press the corresponding A-G key for the card you want to use
* The AI will then play its turn automatically



<!-- CONTACT -->
## Contact

Email: [tim.jameswork9800@gmail.com](mailto:tim.jameswork9800@gmail.com "tim.jameswork9800@gmail.com")

Project Link: [https://github.com/Tim-W-James/Sushi-Go-AI](https://github.com/Tim-W-James/Sushi-Go-AI)



<!-- ACKNOWLEDGEMENTS -->
## Acknowledgements

* Australian National University for project skeleton including terminal interface





<!-- MARKDOWN LINKS & IMAGES -->
<!-- https://www.markdownguide.org/basic-syntax/#reference-style-links -->
[release-shield]: https://img.shields.io/github/v/release/Tim-W-James/repo_name.svg?include_prereleases&style=for-the-badge
[release-url]: https://github.com/Tim-W-James/repo_name/releases
[last-commit-shield]: https://img.shields.io/github/last-commit/Tim-W-James/repo_name.svg?style=for-the-badge
[last-commit-url]: https://github.com/Tim-W-James/repo_name/commits/main
[contributors-shield]: https://img.shields.io/github/contributors/Tim-W-James/repo_name.svg?style=for-the-badge
[contributors-url]: https://github.com/Tim-W-James/repo_name/graphs/contributors
[contributors-shield]: https://img.shields.io/github/contributors/Tim-W-James/repo_name.svg?style=for-the-badge
[contributors-url]: https://github.com/Tim-W-James/repo_name/graphs/contributors
[forks-shield]: https://img.shields.io/github/forks/Tim-W-James/repo_name.svg?style=for-the-badge
[forks-url]: https://github.com/Tim-W-James/repo_name/network/members
[stars-shield]: https://img.shields.io/github/stars/Tim-W-James/repo_name.svg?style=for-the-badge
[stars-url]: https://github.com/Tim-W-James/repo_name/stargazers
[issues-shield]: https://img.shields.io/github/issues/Tim-W-James/repo_name.svg?style=for-the-badge
[issues-url]: https://github.com/Tim-W-James/repo_name/issues
[license-shield]: https://img.shields.io/github/license/Tim-W-James/repo_name?style=for-the-badge
[license-url]: https://github.com/Tim-W-James/repo_name/blob/main/LICENSE.txt
[linkedin-shield]: https://img.shields.io/badge/-LinkedIn-black.svg?style=for-the-badge&logo=linkedin&colorB=555
[linkedin-url]: https://linkedin.com/in/timothy-william-james/
[product-screenshot]: sushiGo.png

<!-- USEFUL LINKS FOR MARKDOWN
* https://www.markdownguide.org/basic-syntax
* https://www.webpagefx.com/tools/emoji-cheat-sheet
* https://shields.io
* https://choosealicense.com
* https://pages.github.com
* https://daneden.github.io/animate.css
* https://connoratherton.com/loaders
* https://kenwheeler.github.io/slick
* https://github.com/cferdinandi/smooth-scroll
* http://leafo.net/sticky-kit
* http://jvectormap.com
* https://fontawesome.com -->
