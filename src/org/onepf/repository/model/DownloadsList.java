package org.onepf.repository.model;

import org.onepf.repository.utils.responsewriter.descriptors.DownloadDescriptor;

import java.util.List;

/**
 * Created by ivanoff on 12.03.14.
 */
public abstract class DownloadsList {

    public abstract List<DownloadDescriptor> getDownloads(Options options);
}
