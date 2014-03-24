package org.onepf.repository;

import org.apache.commons.io.IOUtils;
import org.onepf.repository.model.FileType;
import org.onepf.repository.model.ObjectToDownload;
import org.onepf.repository.utils.downloader.NoPackageException;

import javax.servlet.ServletException;
import javax.servlet.http.HttpServletRequest;
import javax.servlet.http.HttpServletResponse;
import java.io.IOException;
import java.io.InputStream;
import java.io.OutputStream;
import java.util.logging.Logger;

/**
 * Created by ivanoff on 11.03.14.
 */
public class DownloadObjectServlet extends BaseServlet {

    Logger logger = Logger.getLogger("");


    //private static final String PATH_APK = "/apk";
    private static final String PATH_APPDF = "/appdf";
    private static final String PATH_DESCRIPTION = "/appdescription";

    private static final String PARAMETER_PACKAGE = "package";

    private ObjectToDownload objectToDownload;

    @Override
    public void init() throws ServletException {
        super.init();
        objectToDownload = getRepositoryFactory().createFileRequester();
    }

    protected void post(HttpServletRequest request, HttpServletResponse response) throws ServletException, IOException {
    }

    protected void get(HttpServletRequest request, HttpServletResponse response) throws ServletException, IOException {
        String packageName = request.getParameter(PARAMETER_PACKAGE);
        if (packageName == null) {
            response.sendError(HttpServletResponse.SC_BAD_REQUEST, "No package defined");
            return;
        }
        FileType fileType = parseRequestType(request.getRequestURI());
        if (fileType == null) {
            response.sendError(HttpServletResponse.SC_BAD_REQUEST);
            return;
        }

        ObjectToDownload.ObjectOptions objectOptions = new ObjectToDownload.ObjectOptions();
        objectOptions.packageName = packageName;
        objectOptions.fileType = fileType;

        InputStream in = null;
        OutputStream out = null;
        try {
            objectToDownload.init(objectOptions);
            String mimeType = null;
            if (objectOptions.fileType == FileType.DESCRIPTION) {
                mimeType = "application/xml";
            } else {
                mimeType = "application/octet-stream";
                response.setHeader("Content-Disposition", "attachment; filename=\"" + objectToDownload.getName() + "\"");
            }
            response.setContentLength((int) objectToDownload.getSize());
            response.setContentType(mimeType);


            in = objectToDownload.getAsStream();
            out = response.getOutputStream();
            IOUtils.copy(in, out);
        } catch (NoPackageException e) {
            response.sendError(HttpServletResponse.SC_NOT_FOUND, e.getMessage() +" package not found");
        } catch (IOException e) {
            response.sendError(HttpServletResponse.SC_INTERNAL_SERVER_ERROR);
        } finally {
            if (in != null) {
                in.close();
            }
            if (out != null) {
                out.close();
            }
        }
    }

    private FileType parseRequestType(String uri) {
        if (uri.endsWith(PATH_APPDF)) {
            return FileType.APPDF;
        }
        if (uri.endsWith(PATH_DESCRIPTION)) {
            return FileType.DESCRIPTION;
        }
        return null;
    }

}
