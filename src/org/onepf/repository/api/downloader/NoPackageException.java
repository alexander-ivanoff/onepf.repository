package org.onepf.repository.api.downloader;

import java.io.FileNotFoundException;

/**
 * Created by ivanoff on 13.03.14.
 *
 * Thrown if corresponding package was not found in storage
 */
public class NoPackageException extends FileNotFoundException {

    public NoPackageException(String packageName) {
        super(packageName);
    }
}
