package nl.deltares.wti.webservice.api.dikesovertopping;

import org.junit.Test;

import java.io.File;
import java.io.FileInputStream;
import java.io.IOException;
import java.io.InputStream;
import java.net.URISyntaxException;
import java.net.URL;

import static junit.framework.TestCase.*;

public class JsonCalculateParserTest {

    @Test
    public void parseCalculateQo() throws IOException, URISyntaxException {
        DikesOvertoppingContentHandler handler = new DikesOvertoppingContentHandler();

        URL currentTestResourceFolder = getClass().getResource("/examples/json/dikesovertopping");
        File exampleDir = new File(currentTestResourceFolder.toURI());
        assertTrue(exampleDir.exists());
        try(InputStream inputStream = new FileInputStream(new File(exampleDir, "calculateqo.json"))){
            JsonCalculateParser parser = new JsonCalculateParser();
            parser.parse(inputStream, handler);
        }
        assertNotNull(handler.getModelFactors());
        assertNotNull(handler.getGeometry());
        assertNotNull(handler.getLoad());

    }

    @Test
    public void parseReverseCalculate() throws IOException, URISyntaxException {
        DikesOvertoppingContentHandler handler = new DikesOvertoppingContentHandler();

        URL currentTestResourceFolder = getClass().getResource("/examples/json/dikesovertopping");
        File exampleDir = new File(currentTestResourceFolder.toURI());
        assertTrue(exampleDir.exists());
        try(InputStream inputStream = new FileInputStream(new File(exampleDir, "reverse.json"))){
            JsonCalculateParser parser = new JsonCalculateParser();
            parser.parse(inputStream, handler);
        }
        assertNotNull(handler.getModelFactors());
        assertNotNull(handler.getGeometry());
        assertNotNull(handler.getLoad());
        assertEquals(1.1, handler.getDischarge(), 0.0);

    }

    @Test
    public void parseValidate() throws IOException, URISyntaxException {
        DikesOvertoppingContentHandler handler = new DikesOvertoppingContentHandler();

        URL currentTestResourceFolder = getClass().getResource("/examples/json/dikesovertopping");
        File exampleDir = new File(currentTestResourceFolder.toURI());
        assertTrue(exampleDir.exists());
        try(InputStream inputStream = new FileInputStream(new File(exampleDir, "validate.json"))){
            JsonCalculateParser parser = new JsonCalculateParser();
            parser.parse(inputStream, handler);
        }
        assertNotNull(handler.getModelFactors());
        assertNotNull(handler.getGeometry());
        assertNull(handler.getLoad());

    }

}
