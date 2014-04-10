package org.onepf.repository.api.downloader;

import java.io.FileNotFoundException;

/**
 * Thrown if corresponding package was not found in storage
 *
 * @author Alexander Ivanoff
 */
public class NoPackageException extends FileNotFoundException {

    public NoPackageException(String packageName) {
        super(packageName);
    }
}
