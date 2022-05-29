import "./App.css";
import concentric from "./images/concentric.png";
import moon from "./images/moon.png";
import colorCircle from "./images/color_circle.png";
import { useState } from "react";

const images = [concentric, moon, colorCircle];

function Image({ src }: { src: string }) {
  return <img src={src} style={{ objectFit: "contain" }} />;
}

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
        <Image src={images[activeImage]} />
      </div>
    </div>
  );
}

export default App;
