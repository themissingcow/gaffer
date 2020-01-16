//////////////////////////////////////////////////////////////////////////
//
//  Copyright (c) 2019, Cinesite VFX Ltd. All rights reserved.
//
//  Redistribution and use in source and binary forms, with or without
//  modification, are permitted provided that the following conditions are
//  met:
//
//      * Redistributions of source code must retain the above
//        copyright notice, this list of conditions and the following
//        disclaimer.
//
//      * Redistributions in binary form must reproduce the above
//        copyright notice, this list of conditions and the following
//        disclaimer in the documentation and/or other materials provided with
//        the distribution.
//
//      * Neither the name of Cinesite VFX Ltd. nor the names of
//        any other contributors to this software may be used to endorse or
//        promote products derived from this software without specific prior
//        written permission.
//
//  THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS
//  IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO,
//  THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR
//  PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT OWNER OR
//  CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL,
//  EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO,
//  PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR
//  PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF
//  LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING
//  NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS
//  SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
//
//////////////////////////////////////////////////////////////////////////

#ifndef IECOREGLPREVIEW_VISUALISER_H
#define IECOREGLPREVIEW_VISUALISER_H

#include "IECoreGL/Renderable.h"

#include <array>

namespace IECoreGLPreview
{

// We struggled to come up with names for all the combination of behaviours
// that visualisations need. We also wanted to keep the relatively straight
// forward interface that the fixed Visualisations array gives the developer
// implementing a simple visualiser. The compromise was to structure the
// array via a bitmask of behaviours, rather than a list of well-known types.
// We then define additional enum values for well-known combinations of
// behaviours to simplify common uses.
//
// Developers can simply assign to one of the common indices, eg: Geometry,
// or, build custom combinations of behaviours via bitwise operations if
// desired.

enum VisualisationType
{
	// Properties

	InheritLocalScaling = 1,
	// Visualiser scaling can be controlled globally and per-location depending
	// on scene scale or other factors.
	InheritVisualiserScaling = 2,
	// If set, the bound of the visualisation will be considered when 'fit'
	// framing a location. Visualisation bounds are ignored whenever actual
	// geometry is present at a location and it's bound is used instead.
	AffectsFramingBound = 4,

	// Well-known visualisation types

	// Representations of in-world renderable items (eg: VDB).
	Geometry = InheritLocalScaling | AffectsFramingBound,
	// Representations of frustums that have world-meaningful scales.
	Frustum = InheritLocalScaling,
	// Representations of non-renderable visual aids, such as arrows or colors.
	Ornament = InheritVisualiserScaling | AffectsFramingBound,
	// Representations of abstract frustums such as light projections.
	OrnamentFrustum = InheritVisualiserScaling,
};

// A container for renderables grouped by VisualisationType bits.
using Visualisations = std::array<IECoreGL::ConstRenderablePtr, 8>;

namespace Private
{

// Appends any visualisations in source to target. In order to avoid
// over-nesting creating redundant GL state push/pops, it is assumed that target
// is a 'collector' map. And as such, it is safe to append any outer groups in
// source as direct children of the root group of each visualisation type.
void collectVisualisations( const Visualisations &source, Visualisations &target );

} // namespace Private

} // namespace IECoreGLPreview

#endif // IECOREGLPREVIEW_VISUALISER_H
