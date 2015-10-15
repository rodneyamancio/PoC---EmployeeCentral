<?xml version="1.0" encoding="UTF-8"?>
<xsl:stylesheet version="1.0" 
	xmlns:xsl="http://www.w3.org/1999/XSL/Transform" 
	xmlns:ns0="urn:hrsolutions:successfactors:1.0" 
	xmlns="urn:fault.sfapi.successfactors.com" 
	xmlns:ns2="urn:sfobject.sfapi.successfactors.com" 
	xmlns:map="java:java.util.Map" 
	xmlns:dyn="java:com.sap.aii.mapping.api.DynamicConfiguration" 
	xmlns:key="java:com.sap.aii.mapping.api.DynamicConfigurationKey">
 
   <xsl:output method="xml" version="1.0" encoding="UTF-8" indent="yes"/>
   <xsl:param name="sessionId">
      	<xsl:value-of select="//sessionId"/>
   </xsl:param>
   <xsl:param name="inputparam"/>
   <xsl:template match="/*">
            	
      	<!-- change dynamic configuration -->
            	<xsl:variable name="dynamic-conf" select="map:get($inputparam, 'DynamicConfiguration')"/>
            	<xsl:variable name="dynamic-key" select="key:create('http://sap.com/xi/System/HTTP', 'Cookie')"/>
            	<xsl:variable name="dummy" select="dyn:put($dynamic-conf, $dynamic-key, $sessionId)"/>
            	
      	<ns2:query>
               		<xsl:apply-templates select="query/queryString"/>
               		<xsl:apply-templates select="query/param"/>
         </ns2:query>
   </xsl:template>

   <xsl:template match="param">
      <ns2:param>
         <xsl:apply-templates select="*"/>
      </ns2:param>
   </xsl:template>

   <!-- filter -->
   <xsl:template match="id"/>
   <xsl:template match="*">
      <xsl:element name="ns2:{local-name()}" namespace="urn:sfobject.sfapi.successfactors.com">
         <xsl:apply-templates select="@*|*|text()"/>
      </xsl:element>
   </xsl:template>
</xsl:stylesheet>