import "./App.css";
import concentric from "./images/concentric.png";
import moon from "./images/moon.png";
import colorCircle from "./images/color_circle.png";
import { useState } from "react";

const images = [
  {
    url: concentric,
    name: "Concentric",
  },
  {
    url: moon,
    name: "Moon",
  },
  {
    url: colorCircle,
    name: "Colored system",
  },
];

function App() {
  const [activeImage, setActiveImage] = useState(0);

  return (
    <div
      style={{
        display: "flex",
        flexDirection: "column",
        height: "100%",
        justifyContent: "space-around",
        alignItems: "center",
      }}
    >
      <header>
        <div style={{ margin: "auto", textAlign: "center" }}>
          <h1>Generert</h1>
        </div>
      </header>

      <div
        style={{
          display: "flex",
          justifyContent: "center",
          border: "1px solid black",
          borderRadius: "5px",
          padding: "1vh",
          height: "80%",
          width: "80%",
          textAlign: "center",
        }}
        onClick={() => setActiveImage((activeImage + 1) % images.length)}
      >
        <img src={images[activeImage].url} alt={images[activeImage].name} />
      </div>
    </div>
  );
}

export default App;
