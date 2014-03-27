package org.onepf.repository.model.services.filesystem;

import javax.servlet.ServletContext;

/**
 * Created by ivanoff on 25.03.14.
 */
public class FilesystemOptions {

    // TODO maybe move all setting to separate .properties file
    public String targetDir = "/packages/";
    public String targetPath; //

    public FilesystemOptions(ServletContext context) {
        targetPath = context.getRealPath(targetDir);
    }


}
