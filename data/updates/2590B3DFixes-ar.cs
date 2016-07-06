'From Squeak2.9alpha of 12 June 2000 [latest update: #2589] on 9 September 2000 at 11:20:45 pm'!"Change Set:		B3DFixes-arDate:			9 September 2000Author:			Andreas RaabThe change set fixes a very bad problem in the shader and makes alpha blends behave correctly. The shader modification in this change set is not installed (so that current images will continue to run). Installation is part of a later update but can be manually executed by evaluating:	B3DPoolDefinier initPool."!!B3DPoolDefiner class methodsFor: 'pool definition' stamp: 'ar 9/9/2000 23:16'!defineMaterialAndLights: dict	"Initialize constants used for materials and lights"	"B3DPoolDefiner initPool"	self initFromSpecArray:	#(		"MaterialColor stuff"		(AmbientPart 0)		(AmbientRed 0)		(AmbientGreen 1)		(AmbientBlue 2)		(AmbientAlpha 3)		(DiffusePart 4)		(DiffuseRed 4)		(DiffuseGreen 5)		(DiffuseBlue 6)		(DiffuseAlpha 7)		(SpecularPart 8)			(SpecularRed 8)		(SpecularGreen 9)		(SpecularBlue 10)		(SpecularAlpha 11)		(MaterialColorSize 12)	"Size of B3DMaterialColor"		"Material definition"		(EmissionPart 12)		(EmissionRed 12)		(EmissionGreen 13)		(EmissionBlue 14)		(EmissionAlpha 15)		(MaterialShininess 16)		(MaterialSize 17)			"Size of B3DMaterial"		"PrimitiveLight definition"		(PrimLightPosition 12)		(PrimLightPositionX 12)		(PrimLightPositionY 13)		(PrimLightPositionZ 14)		(PrimLightDirection 15)		(PrimLightDirectionX 15)		(PrimLightDirectionY 16)		(PrimLightDirectionZ 17)		(PrimLightAttenuation 18)		(PrimLightAttenuationConstant 18)		(PrimLightAttenuationLinear 19)		(PrimLightAttenuationSquared 20)		(PrimLightFlags 21)		"Spot light stuff"		(SpotLightMinCos 22)		(SpotLightMaxCos 23)		(SpotLightDeltaCos 24)		(SpotLightExponent 25)		(PrimLightSize 32)		"Round up to power of 2"		"Primitive light flags"		(FlagPositional 16r0001)	"Light has an associated position"		(FlagDirectional 16r0002)	"Light has an associated direction"		(FlagAttenuated 16r0004)	"Light is attenuated"		(FlagHasSpot 16r0008)	"Spot values are valid"		(FlagAmbientPart 16r0100)	"Light has ambient part"		(FlagDiffusePart 16r0200)	"Light has diffuse part"		(FlagSpecularPart 16r0400)	"Light has specular part"	) in: dict.! !!BitBltSimulation methodsFor: 'combination rules' stamp: 'ar 9/9/2000 23:17'!alphaBlend: sourceWord with: destinationWord	"Blend sourceWord with destinationWord, assuming both are 32-bit pixels.	The source is assumed to have 255*alpha in the high 8 bits of each pixel,	while the high 8 bits of the destinationWord will be ignored.	The blend produced is alpha*source + (1-alpha)*dest, with	the computation being performed independently on each color	component.  The high byte of the result will be 0."	| alpha unAlpha colorMask result blend shift |	self inline: false.	alpha _ sourceWord >> 24.  "High 8 bits of source pixel"	unAlpha _ 255 - alpha.	colorMask _ 16rFF.	result _ 0.	"ar 9/9/2000 - include alpha in computation"	1 to: 4 do:		[:i | shift _ (i-1)*8.		blend _ (((sourceWord>>shift bitAnd: colorMask) * alpha)					+ ((destinationWord>>shift bitAnd: colorMask) * unAlpha))			 	+ 254 // 255 bitAnd: colorMask.		result _ result bitOr: blend<<shift].	^ result! !!FXBltSimulation methodsFor: 'combination rules' stamp: 'ar 9/9/2000 23:17'!alphaBlend: sourceWord with: destinationWord	"Blend sourceWord with destinationWord, assuming both are 32-bit pixels.	The source is assumed to have 255*alpha in the high 8 bits of each pixel,	while the high 8 bits of the destinationWord will be ignored.	The blend produced is alpha*source + (1-alpha)*dest, with	the computation being performed independently on each color	component.  The high byte of the result will be 0."	| alpha unAlpha colorMask result blend shift |	self inline: false.	alpha _ sourceWord >> 24.  "High 8 bits of source pixel"	unAlpha _ 255 - alpha.	colorMask _ 16rFF.	result _ 0.	"ar 9/9/2000 - include alpha in computation"	1 to: 4 do:		[:i | shift _ (i-1)*8.		blend _ (((sourceWord>>shift bitAnd: colorMask) * alpha)					+ ((destinationWord>>shift bitAnd: colorMask) * unAlpha))			 	+ 254 // 255 bitAnd: colorMask.		result _ result bitOr: blend<<shift].	^ result! !