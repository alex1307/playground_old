package com.ayagasha.playground.devops.files;

import com.ayagasha.playground.devops.lambdas.ByteUnit;
import com.ayagasha.playground.devops.lambdas.FileAssembler;
import com.ayagasha.playground.devops.lambdas.FileUtils;
import org.junit.Before;
import org.junit.Test;

import java.io.IOException;
import java.nio.file.Files;
import java.nio.file.Path;
import java.nio.file.Paths;
import java.nio.file.attribute.PosixFileAttributes;
import java.nio.file.attribute.PosixFilePermission;
import java.nio.file.attribute.PosixFilePermissions;
import java.util.Set;

import static junit.framework.TestCase.assertTrue;


public class TestFileAssembler {


    FileAssembler executor;
    private static final String dir = "files/c7d37566-6348-4167-8bc7-ca28ee1e8221";
    private static final String targetDir = "files/c7d37566-6348-4167-8bc7-ca28ee1e8221/target";
    private static final Path path = Paths.get(targetDir + "/test.txt");

    @Before
    public void init() throws IOException {
        if (Files.exists(Paths.get(targetDir))) {
            FileUtils.deleteDir(targetDir);
        }
    }

    @Test
    public void testTxtAssembler() throws Exception {
        FileUtils.createDir(targetDir);
        executor = new FileAssembler(dir, path.toAbsolutePath().toString(), 4L, ByteUnit.KILOBYTE);
        Boolean success = executor.execute();
        assertTrue(success);

        Set<PosixFilePermission> perms = Files.readAttributes(path, PosixFileAttributes.class).permissions();

        System.out.format("Permissions before: %s%n",  PosixFilePermissions.toString(perms));
    }

    @Test
    public void testImgAssembler() throws Exception {
        Path path = Paths.get("files/1d7a1c36-efff-44fd-9c99-ef07640b46c2/target/img.tiff");
        FileUtils.createDir("files/1d7a1c36-efff-44fd-9c99-ef07640b46c2/target");
        executor = new FileAssembler("files/1d7a1c36-efff-44fd-9c99-ef07640b46c2/", path.toAbsolutePath().toString(), 1L, ByteUnit.MEGABYTE);
        Boolean success = executor.execute();
        assertTrue(success);

        Set<PosixFilePermission> perms = Files.readAttributes(path, PosixFileAttributes.class).permissions();

        System.out.format("Permissions before: %s%n",  PosixFilePermissions.toString(perms));
    }
}
