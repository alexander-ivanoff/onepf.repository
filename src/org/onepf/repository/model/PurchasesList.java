package org.onepf.repository.model;

import org.onepf.repository.utils.responsewriter.descriptors.PurchaseDescriptor;

import java.util.List;

/**
 * Created by ivanoff on 12.03.14.
 */
public abstract class PurchasesList {

    public abstract List<PurchaseDescriptor> getPurchases(Options options);
}
