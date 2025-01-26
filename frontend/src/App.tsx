import "./App.css";
import { useState } from "react";
import texture from "../../images/texture-15128758_0.43535608.png";
import texture2 from "../../images/texture-15128758_0.058291435.png";
import concentric from "../../images/concentric/concentric-15128758_0.9700706.png";
import twoLines from "../../images/two-lines/two-lines-15128758_0.68674845.png";
import nonIntersectingLines from "../../images/line_intersections/15128758_0.57497156.png";
import githubIcon from "./githubIcon.svg";

function Header() {
  return (
    <div className="header">
      <h1>Generert</h1>
    </div>
  );
}

type Image = {
  name: string;
  url: string;
  description: string;
};

const images: Image[] = [
  {
    name: "Texture",
    url: texture,
    description: "Random lines placed in circles.",
  },
  {
    name: "Moon",
    url: texture2,
    description: "Random lines placed in a circle.",
  },
  {
    name: "Contentric",
    url: concentric,
    description: "Concentric circles drawn with some randomness.",
  },
  { name: "Two Lines", url: twoLines, description: "Lines between two lines." },
  {
    name: "Non-intersecting lines",
    url: nonIntersectingLines,
    description:
      "Many random lines on a circle with non-intersecting condition.",
  },
];

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
