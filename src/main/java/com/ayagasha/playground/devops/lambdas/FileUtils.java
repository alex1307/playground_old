package com.ayagasha.playground.devops.lambdas;

import lombok.extern.slf4j.Slf4j;

import java.io.IOException;
import java.nio.file.Files;
import java.nio.file.Path;
import java.nio.file.Paths;

@Slf4j
public class FileUtils {

    public static String getDirectory(final String fileName) throws IOException {

        final Path filePath = Paths.get(fileName);
        if (Files.exists(filePath)) {

            if (Files.isDirectory(filePath))
                return filePath.toAbsolutePath().toString();

            return filePath.toAbsolutePath().getParent().toString();

        } else
            throw new IOException(String.format("File %s does not exist", fileName));

    }

    public static boolean createDir(final String dirName) throws IOException {
        final Path filePath = Paths.get(dirName);

        if (Files.exists(filePath)) {
            throw new IOException(String.format("Directory %s already exists", dirName));
        }

        final boolean status = filePath.toFile().mkdir();
        if (status)
            log.info("Directory {} has been created successfully.", filePath.toAbsolutePath());

        return status;
    }

    public static boolean deleteDir(final String dirName) throws IOException {

        final Path filePath = Paths.get(dirName);
        if (!Files.exists(filePath)) {
            throw new IOException(String.format("Directory %s does not exist", dirName));
        }
        final boolean status =  filePath.toFile().delete();
        if (status)
            log.info("Directory {} has been deleted successfully.", filePath.toAbsolutePath());

        return status;
    }

    public static long fileSize(final String fileName) throws IOException{
        final Path path = Paths.get(fileName);
        if(Files.exists(path)) {
            return Files.size(path);
        }
        throw new IOException(String.format("File %s does not exist", fileName));
    }
}
