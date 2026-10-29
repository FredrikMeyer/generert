import "./App.css";
import { useState } from "react";
import publishedImages from "virtual:published-images";
import githubIcon from "./githubIcon.svg";

function Header() {
  return (
    <div className="header">
      <h1>Generert</h1>
    </div>
  );
}

type Image = (typeof publishedImages)[number];

const images: Image[] = publishedImages;

function Gallery({
  setActiveImage,
}: {
  setActiveImage: (image: Image) => void;
}) {
  return (
    <div className="images">
      {images.map((i) => (
        <div key={i.url} onClick={() => setActiveImage(i)} className="image">
          <img src={i.url} alt={i.description} className="image-img" />
        </div>
      ))}
    </div>
  );
}

function SingleImage({ close, image }: { image: Image; close: () => void }) {
  return (
    <div
      onClick={() => close()}
      style={{
        display: "flex",
        alignItems: "center",
        justifyContent: "center",
      }}
    >
      <figure>
        <img src={image.url} alt={image.description} />
        <figcaption>
          <b>{image.name}</b>: {image.description}
        </figcaption>
      </figure>
    </div>
  );
}

function App() {
  const [activeImage, setActiveImage] = useState<null | Image>(null);

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
