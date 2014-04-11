package org.onepf.repository.model.services.filesystem;

import org.onepf.repository.model.services.StorageService;
import org.onepf.repository.model.services.StorageServiceOptions;

import javax.servlet.ServletContext;

/**
 * Implementation of StorageServiceOptions for FilesystemStorageService
 *
 * @author Alexander Ivanoff
 * @see org.onepf.repository.model.services.filesystem.FilesystemStorageService
 */
public class FilesystemOptions implements StorageServiceOptions {

    public static final String SERVICE_NAME= "filesystem";

    // TODO maybe move all setting to separate .properties file
    public String targetDir = "/packages/";
    public String targetPath; //

    public FilesystemOptions(ServletContext context) {
        targetPath = context.getRealPath(targetDir);
    }


    @Override
    public StorageService createStorageService() {
        return new FilesystemStorageService(this);
    }
}
