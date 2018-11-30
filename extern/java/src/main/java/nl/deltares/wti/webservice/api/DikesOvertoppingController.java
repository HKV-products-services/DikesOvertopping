package nl.deltares.wti.webservice.api;

import nl.deltares.wti.kernels.dikesovertopping.OvertoppingResult;
import nl.deltares.wti.webservice.api.dikesovertopping.DikesOvertoppingContentHandler;
import nl.deltares.wti.webservice.api.dikesovertopping.DikesOvertoppingService;
import nl.deltares.wti.webservice.api.dikesovertopping.JsonCalculateParser;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.ResponseBody;
import org.springframework.web.bind.annotation.RestController;
import org.springframework.web.context.request.ServletWebRequest;
import org.springframework.web.context.request.WebRequest;

import javax.servlet.http.HttpServletRequest;
import java.io.IOException;

import static org.springframework.http.MediaType.APPLICATION_JSON_VALUE;
import static org.springframework.web.bind.annotation.RequestMethod.POST;

@RestController
@RequestMapping(value = "/api/1.0/dikesovertopping")
public class DikesOvertoppingController {

    final DikesOvertoppingService service;

    @Autowired
    public DikesOvertoppingController(DikesOvertoppingService service) {
        this.service = service;
    }

    @RequestMapping(value = "/calculateDischarge", method = POST, consumes = APPLICATION_JSON_VALUE, produces = APPLICATION_JSON_VALUE)
    @ResponseBody
    public OvertoppingResult calculateDischarge(WebRequest request) {
        HttpServletRequest incomming = ((ServletWebRequest) request).getRequest();

        JsonCalculateParser parser = new JsonCalculateParser();
        DikesOvertoppingContentHandler contentHandler = new DikesOvertoppingContentHandler();
        try {
            parser.parse(incomming.getInputStream(), contentHandler);
        } catch (IOException e) {
            throw new RuntimeException("Error parsing request content: " + e.getMessage(), e);
        }

        return service.calculateDischarge(contentHandler.getLoad(), contentHandler.getGeometry(), contentHandler.getModelFactors());
    }

    @RequestMapping(value = "/calculateHeight", method = POST, consumes = APPLICATION_JSON_VALUE, produces = APPLICATION_JSON_VALUE)
    @ResponseBody
    public OvertoppingResult calculateHeight(WebRequest request) {
        HttpServletRequest incomming = ((ServletWebRequest) request).getRequest();

        JsonCalculateParser parser = new JsonCalculateParser();
        DikesOvertoppingContentHandler contentHandler = new DikesOvertoppingContentHandler();
        try {
            parser.parse(incomming.getInputStream(), contentHandler);
        } catch (IOException e) {
            throw new RuntimeException("Error parsing request content: " + e.getMessage(), e);
        }

        return service.calculateHeight(contentHandler.getLoad(), contentHandler.getGeometry(), contentHandler.getModelFactors(), contentHandler.getDischarge());

    }

    @RequestMapping(value = "/validate", method = POST, consumes = APPLICATION_JSON_VALUE, produces = APPLICATION_JSON_VALUE)
    @ResponseBody
    public OvertoppingResult valid(WebRequest request) {
        HttpServletRequest incomming = ((ServletWebRequest) request).getRequest();

        JsonCalculateParser parser = new JsonCalculateParser();
        DikesOvertoppingContentHandler contentHandler = new DikesOvertoppingContentHandler();
        try {
            parser.parse(incomming.getInputStream(), contentHandler);
        } catch (IOException e) {
            throw new RuntimeException("Error parsing request content: " + e.getMessage(), e);
        }

        return service.validate(contentHandler.getGeometry(), contentHandler.getModelFactors());
    }

}
