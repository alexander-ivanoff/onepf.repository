package org.onepf.repository.api.responsewriter.descriptors;

import org.onepf.repository.api.responsewriter.ResponseWriter;
import org.onepf.repository.api.responsewriter.WritableHeader;
import org.onepf.repository.api.responsewriter.WriteException;

/**
 * Created by ivanoff on 03.04.14.
 */
public class DownloadsListHeaderDescriptor extends  BaseListHeaderDescriptor implements WritableHeader {

    public DownloadsListHeaderDescriptor(String version, String offset) {
        super(version, offset);
    }

    @Override
    public void writeOpening(ResponseWriter responseWriter) throws WriteException {
        responseWriter.writeOpening(this);
    }

    @Override
    public void writeClosing(ResponseWriter responseWriter) throws WriteException {
        responseWriter.writeClosing(this);
    }
}
