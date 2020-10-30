package com.ayagasha.playground.infrastructure;

import lombok.extern.slf4j.Slf4j;
import org.junit.Test;
import org.junit.jupiter.api.Assertions;
import org.junit.runner.RunWith;
import org.springframework.boot.test.context.SpringBootTest;
import org.springframework.test.context.ActiveProfiles;
import org.springframework.test.context.junit4.SpringRunner;

@RunWith(SpringRunner.class)
@SpringBootTest
@ActiveProfiles("test")
@Slf4j
public class LiquibaseMigrationsTest {

    @Test
    public void loadLiquibaseMigrationTest() {
        Assertions.assertTrue(true);
        log.info("SUCCESS. Migrations scripts are executed successfully");
    }

}
