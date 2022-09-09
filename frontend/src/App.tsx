import "./App.css";
import { useState } from "react";
import texture from "../../images/texture-15128758_0.43535608.png";
import texture2 from "../../images/texture-15128758_0.058291435.png";
import concentric from "../../images/concentric-15128758_0.9700706.png";
import twoLines from "../../images/two-lines-15128758_0.68674845.png";
import githubIcon from "./githubIcon.svg";

function Header() {
  return (
    <div className="header">
      <h1>Generert</h1>
    </div>
  );
}

function Gallery({
  setActiveImage,
}: {
  setActiveImage: (image: string) => void;
}) {
  const images = [texture, texture2, concentric, twoLines];
  return (
    <div className="images">
      {images.map((i) => (
        <div key={i} onClick={() => setActiveImage(i)} className="image">
          <img
            src={i}
            alt={i}
            style={{
              borderRadius: "50%",
              objectFit: "cover",
              width: "100%",
              maxHeight: "100%",
            }}
          />
        </div>
      ))}
    </div>
  );
}

function SingleImage({ close, image }: { image: string; close: () => void }) {
  return (
    <div
      onClick={() => close()}
      style={{
        display: "flex",
        alignItems: "center",
        justifyContent: "center",
      }}
    >
      <img src={image} alt={image} />
    </div>
  );
}

function App() {
  const [activeImage, setActiveImage] = useState<null | string>(null);

  return (
    <div
      style={{
        display: "flex",
        flexDirection: "column",
        height: "100vh",
        justifyContent: "space-between",
        maxHeight: "100vh",
      }}
    >
      <Header />
      <main>
        {activeImage ? (
          <SingleImage close={() => setActiveImage(null)} image={activeImage} />
        ) : (
          <Gallery setActiveImage={setActiveImage} />
        )}
      </main>
      <div className="footer">
        <div>Fredrik Meyer</div>
        <div>
          <a href="https://github.com/FredrikMeyer/generert">
            <img src={githubIcon} alt="Github Icon" />
          </a>
        </div>
      </div>
    </div>
  );
}

export default App;
