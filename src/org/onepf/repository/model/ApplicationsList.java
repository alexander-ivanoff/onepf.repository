package org.onepf.repository.model;

import org.onepf.repository.utils.responsewriter.descriptors.ApplicationDescriptor;

import java.util.List;

/**
 * Created by ivanoff on 12.03.14.
 */
public abstract class ApplicationsList {

    public abstract List<ApplicationDescriptor> getApplications(Options options);
}
