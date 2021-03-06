package org.onepf.repository.api.responsewriter.entity;

import javax.persistence.*;
import javax.xml.bind.annotation.XmlAccessType;
import javax.xml.bind.annotation.XmlAccessorType;
import javax.xml.bind.annotation.XmlAttribute;

/**
 * Created by akarimova on 16.04.14.
 */

@MappedSuperclass
@XmlAccessorType(XmlAccessType.NONE)
public abstract class BaseStatisticEntity extends BaseHashEntity{
    @Id
    @GeneratedValue(strategy= GenerationType.AUTO)
    private int id;

    @XmlAttribute(name = "package", required = true)
    @Column(name = "package")
    private String packageName;

    @XmlAttribute(name = "datetime")
    @Column(name = "datetime")
    private String dateTime;

    @XmlAttribute(name = "version")
    @Column(name = "version")
    private String version;

    @XmlAttribute(name = "versionCode", required = true)
    @Column(name = "versionCode")
    private int build;

    @XmlAttribute(name = "device-model", required = true)
    @Column(name = "deviceModel")
    private String deviceModel;

    @XmlAttribute(name = "device-name")
    @Column(name = "deviceName")
    private String deviceName;

    @XmlAttribute(name = "country", required = true)
    @Column(name = "country")
    private String country;

    @XmlAttribute(name = "appstoreId", required = true)
    @Column(name = "distributorStoreId")
    private String distributorStoreId;

    @Column(name = "homeStoreId")
    private String homeStoreId;

    public String getPackageName() {
        return packageName;
    }

    public void setPackageName(String packageName) {
        this.packageName = packageName;
    }

    public String getDateTime() {
        return dateTime;
    }

    public void setDateTime(String dateTime) {
        this.dateTime = dateTime;
    }

    public String getVersion() {
        return version;
    }

    public void setVersion(String version) {
        this.version = version;
    }

    public int getBuild() {
        return build;
    }

    public void setBuild(int build) {
        this.build = build;
    }

    public String getDeviceModel() {
        return deviceModel;
    }

    public void setDeviceModel(String deviceModel) {
        this.deviceModel = deviceModel;
    }

    public String getDeviceName() {
        return deviceName;
    }

    public void setDeviceName(String deviceName) {
        this.deviceName = deviceName;
    }

    public String getCountry() {
        return country;
    }

    public void setCountry(String country) {
        this.country = country;
    }

    public int getId() {
        return id;
    }

    public void setId(int id) {
        this.id = id;
    }

    public String getDistributorStoreId() {
        return distributorStoreId;
    }

    public void setDistributorStoreId(String distributorStoreId) {
        this.distributorStoreId = distributorStoreId;
    }

    public String getHomeStoreId() {
        return homeStoreId;
    }

    public void setHomeStoreId(String homeStoreId) {
        this.homeStoreId = homeStoreId;
    }
}
