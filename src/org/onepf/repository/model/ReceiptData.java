package org.onepf.repository.model;

import com.fasterxml.jackson.annotation.JsonProperty;

/**
 * Created by ivanoff on 06.05.14.
 */
public class ReceiptData {

    @JsonProperty("appstoreId")
    public String appstoreId;

    @JsonProperty("orderId")
    public String orderId;

    @JsonProperty("packageName")
    public String packageName;

    @JsonProperty("productId")
    public String productId;

    @JsonProperty("purchaseTime")
    public String purchaseTime;

    @JsonProperty("purchaseToken")
    public String purchaseToken;

    @JsonProperty("developerPayload")
    public String developerPayload;

}
