package com.ayagasha.playground.devops.files;

import com.ayagasha.playground.devops.lambdas.FileUtils;
import org.junit.FixMethodOrder;
import org.junit.Test;
import org.junit.runners.MethodSorters;

import java.io.IOException;

import static org.junit.Assert.assertNotNull;
import static org.junit.Assert.assertTrue;

@FixMethodOrder(value = MethodSorters.NAME_ASCENDING)
public class TestFileUtils {

    final String dirName = "delete_it_1234";

    @Test
    public void testDirectory() throws IOException {

        String fileName = "/Users/alex/Downloads/Zoom.pkg";
        String directory = FileUtils.getDirectory(fileName);

        assertNotNull(directory);

    }

    @Test
    public void testCreateDirectory() throws IOException {
        assertTrue(FileUtils.createDir(dirName));
    }

    @Test
    public void testDeleteDir() throws IOException {
        assertTrue(FileUtils.deleteDir(dirName));
    }


}
