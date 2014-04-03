package org.onepf.repository.utils.responsewriter.descriptors;

import org.onepf.repository.utils.responsewriter.ResponseWriter;
import org.onepf.repository.utils.responsewriter.WritableHeader;
import org.onepf.repository.utils.responsewriter.WriteException;

/**
 * Created by ivanoff on 03.04.14.
 */
public class ReviewsListHeaderDescriptor extends  BaseListHeaderDescriptor implements WritableHeader {

    public ReviewsListHeaderDescriptor(String version, String offset) {
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
