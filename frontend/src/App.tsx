import "./App.css";
import concentric from "./images/concentric.png";
import moon from "./images/moon.png";
import colorCircle from "./images/color_circle.png";
import { Container, SimpleGrid, Text, Center, Box } from "@chakra-ui/react";
import { useState } from "react";

const images = [concentric, moon, colorCircle];

function Image({ src }: { src: string }) {
  return (
    <Center>
      <img src={src} />
    </Center>
  );
}

function App() {
  const [activeImage, setActiveImage] = useState(0);

  return (
    <SimpleGrid minH="100vh" gap="0" paddingTop="1vh" paddingBottom="1vh">
      <Container alignItems="center">
        <Center>
          <Text fontSize="6xl">Generert</Text>
        </Center>
      </Container>
      <Box padding="20px">
        <Container
          border="1px solid black"
          borderRadius="5px"
          padding="1vh"
          maxW="container.sm"
          onClick={() => setActiveImage((activeImage + 1) % images.length)}
        >
          <Image src={images[activeImage]} />
        </Container>
      </Box>
    </SimpleGrid>
  );
}

export default App;
