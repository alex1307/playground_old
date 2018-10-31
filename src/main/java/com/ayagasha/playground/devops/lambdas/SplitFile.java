package com.ayagasha.playground.devops.lambdas;

import lombok.Builder;
import lombok.Data;


@Builder
@Data
public class SplitFile {
    private Long index;
    private Long byteSize;
    private String filePath;
}
