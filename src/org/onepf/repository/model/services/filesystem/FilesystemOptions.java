package org.onepf.repository.model.services.filesystem;

import org.onepf.repository.model.services.StorageService;
import org.onepf.repository.model.services.StorageServiceOptions;

import javax.servlet.ServletContext;

/**
 * Created by ivanoff on 25.03.14.
 */
public class FilesystemOptions implements StorageServiceOptions {

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
