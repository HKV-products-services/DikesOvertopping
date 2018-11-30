package nl.deltares.wti;

import org.springframework.boot.SpringApplication;
import org.springframework.boot.autoconfigure.SpringBootApplication;
import org.springframework.boot.autoconfigure.jdbc.DataSourceAutoConfiguration;
import org.springframework.boot.autoconfigure.jdbc.DataSourceTransactionManagerAutoConfiguration;
import org.springframework.boot.autoconfigure.orm.jpa.HibernateJpaAutoConfiguration;
import org.springframework.boot.autoconfigure.web.servlet.MultipartAutoConfiguration;
import org.springframework.boot.web.servlet.support.SpringBootServletInitializer;

import java.util.Properties;
import java.util.TimeZone;

@SpringBootApplication(exclude = {DataSourceAutoConfiguration.class, DataSourceTransactionManagerAutoConfiguration.class, HibernateJpaAutoConfiguration.class,  MultipartAutoConfiguration.class})
public class Application extends SpringBootServletInitializer {

    public static void init() {
        TimeZone.setDefault(TimeZone.getTimeZone("GMT"));
    }
    
    public static Properties getSpringProperties() {
        Properties properties = new Properties();
        properties.put("spring.servlet.multipart.enabled", false);
        return properties;
    }
    
    public static void main(String[] args) {
        SpringApplication application = new SpringApplication(Application.class);
        Properties properties = getSpringProperties();
        properties.put("logging.file", "./wti-tools.log");
        application.setDefaultProperties(properties);
        init();
        application.run(args);
    }
}
