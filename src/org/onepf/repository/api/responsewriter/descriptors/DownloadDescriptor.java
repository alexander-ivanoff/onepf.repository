package org.onepf.repository.api.responsewriter.descriptors;

import org.onepf.repository.api.responsewriter.ResponseWriter;
import org.onepf.repository.api.responsewriter.Writable;
import org.onepf.repository.api.responsewriter.WriteException;

/**
 * Created by ivanoff on 12.03.14.
 */
public class DownloadDescriptor extends AbstractDescriptor implements Writable {
    public String packageName;
    public String dateTime;
    public String isUpdate;
    public String version;
    public int build;
    public String lastUpdate;
    public String deviceModel;
    public String deviceName;
    public String country;
    public int currPageHash;
    public int prevPageHash;

    @Override
    public void write(ResponseWriter responseWriter) throws WriteException {
        responseWriter.write(this);
    }
}
