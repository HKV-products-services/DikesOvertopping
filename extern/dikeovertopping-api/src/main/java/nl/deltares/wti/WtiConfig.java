package nl.deltares.wti;

import org.springframework.context.annotation.Bean;
import org.springframework.context.annotation.Configuration;
import org.springframework.web.servlet.config.annotation.CorsRegistry;
import org.springframework.web.servlet.config.annotation.WebMvcConfigurer;

@Configuration
public class WtiConfig {

    @Bean
    public WebMvcConfigurer corsConfigurer() {
        return new WebMvcConfigurer() {
            @Override
            public void addCorsMappings(CorsRegistry registry) {
                registry.addMapping("/api/**")
                        .allowedOrigins("*")
                        .allowedMethods("POST","GET","OPTIONS")
                        .allowedHeaders("Origin","Accept","X-Requested-With","Content-Type","Access-Control-Request-Method","Access-Control-Request-Headers","Authorization")
                        .allowCredentials(false).maxAge(3600);
            }
        };
    }
}
