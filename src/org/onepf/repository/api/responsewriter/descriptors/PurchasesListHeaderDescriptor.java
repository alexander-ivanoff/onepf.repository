package org.onepf.repository.api.responsewriter.descriptors;

import org.onepf.repository.api.responsewriter.ResponseWriter;
import org.onepf.repository.api.responsewriter.WritableHeader;
import org.onepf.repository.api.responsewriter.WriteException;

/**
 * This class contains description of header of a purchase list
 *
 * @author Alexander Ivanoff
 */
public class PurchasesListHeaderDescriptor extends BaseListHeaderDescriptor implements WritableHeader {

    public PurchasesListHeaderDescriptor(String version, String offset) {
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
