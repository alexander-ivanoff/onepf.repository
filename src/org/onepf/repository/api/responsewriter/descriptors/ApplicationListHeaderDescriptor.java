package org.onepf.repository.api.responsewriter.descriptors;

import org.onepf.repository.api.responsewriter.WriteException;
import org.onepf.repository.api.responsewriter.entity.BaseEntity;

/**
 * This class contains description of header of a application list
 *
 * @author Alexander Ivanoff
 */
public class ApplicationListHeaderDescriptor extends BaseEntity implements WritableHeader {

    public ApplicationListHeaderDescriptor(String version, String offset) {
//        super(version, offset);
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
