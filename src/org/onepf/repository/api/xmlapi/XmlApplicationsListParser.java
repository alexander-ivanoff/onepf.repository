package org.onepf.repository.api.xmlapi;

import org.onepf.repository.api.responsewriter.descriptors.ApplicationDescriptor;
import org.onepf.repository.api.responsewriter.descriptors.ApplicationListHeaderDescriptor;

import java.util.Collection;

/**
 *
 * XmlListParser realization to parse list of applications
 *
 * @author Alexander Ivanoff on 02.04.14.
 */
public class XmlApplicationsListParser extends  XmlListParser<ApplicationDescriptor, ApplicationListHeaderDescriptor> {

    public XmlApplicationsListParser(Collection<ApplicationDescriptor> items) {
        super(items, new XmlApplicationItemParser(), new XmlApplicationHeaderParser());
    }
}
