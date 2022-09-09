import "./App.css";
import { useState } from "react";
import texture from "./images/texture-15128758_0.43535608.png";
import texture2 from "./images/texture-15128758_0.058291435.png";
import concentric from "./images/concentric-15128758_0.9700706.png";

function Header() {
  return (
    <div
      style={{
        borderBottom: "3px double black",
        marginBottom: "5px",
        textAlign: "center",
      }}
    >
      <h1>Generert</h1>
    </div>
  );
}

function Gallery({
  setActiveImage,
}: {
  setActiveImage: (image: string) => void;
}) {
  const images = [texture, texture2, concentric];
  return (
    <div className="images">
      {images.map((i) => (
        <div key={i} onClick={() => setActiveImage(i)} className="image">
          <img
            src={i}
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
      <img src={image} />
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
      <div className="footer">Fredrik Meyer</div>
    </div>
  );
}

export default App;
