#[cfg(HAS_BACKTRACE)]
use std::backtrace::Backtrace;

use std::{collections::HashMap, fmt::Display};

use thiserror::Error;

const HEADER_CHUNK_ID: &[u8; 4] = b"MThd";
const TRACK_CHUNK_ID: &[u8; 4] = b"MTrk";

const BYTE_HIGH_BIT: u8 = 0x80;
const BYTE_LOW_BITS: u8 = 0x7F;

#[derive(Debug, Error)]
struct MidiConvertError {
    msg: Option<String>,
    source: MidiConvertErrorTypes,
    #[cfg(HAS_BACKTRACE)]
    backtrace: Backtrace,
}

impl Display for MidiConvertError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let result = write!(
            f,
            "{}: {}",
            self.source,
            self.msg.as_ref().unwrap_or(&"".into())
        );
        #[cfg(HAS_BACKTRACE)]
        let result = result.and(
            self.backtrace
                .frames()
                .iter()
                .fold(Ok(()), |ok, frame| write!(f, "{:?}", frame).and(ok)),
        );
        result
    }
}

#[derive(Debug, PartialEq, Eq, Error)]
enum MidiConvertErrorTypes {
    #[error("Failed to convert between [u8] and SmpteFps.")]
    Smpte,
    #[error("Invalid conversion from u8 to MidiFormat.")]
    MidiFormat,
    #[error("Invalid chunk id! Must be one of b\"MThd\" or b\"MTrk\"")]
    ChunkId,
    #[error("Header chunk is invalid data")]
    HeaderChunk,
    #[error("Time division is invalid")]
    TimeDivision,
    #[error("Variable length value read error")]
    VariableLength,
    #[error("Bytes were not valid events")]
    EventParse,
}

#[derive(Debug, Clone, Copy, Eq, PartialEq)]
enum ChunkId {
    Header,
    Track,
}

impl TryFrom<&[u8]> for ChunkId {
    type Error = MidiConvertError;

    fn try_from(value: &[u8]) -> Result<Self, Self::Error> {
        if value.len() != 4 {
            return Err(MidiConvertError {
                msg: Some(format!("chunk id is 4 bytes, given {}", value.len())),
                source: MidiConvertErrorTypes::ChunkId,
                #[cfg(HAS_BACKTRACE)]
                backtrace: Backtrace::capture(),
            });
        }
        if value == HEADER_CHUNK_ID {
            Ok(Self::Header)
        } else if value == TRACK_CHUNK_ID {
            Ok(Self::Track)
        } else {
            Err(MidiConvertError {
                msg: Some(format!("Chunk id given was {:?}", value)),
                source: MidiConvertErrorTypes::ChunkId,
                #[cfg(HAS_BACKTRACE)]
                backtrace: Backtrace::capture(),
            })
        }
    }
}

impl From<ChunkId> for &[u8] {
    fn from(chunk_id: ChunkId) -> Self {
        match chunk_id {
            ChunkId::Header => HEADER_CHUNK_ID,
            ChunkId::Track => TRACK_CHUNK_ID,
        }
    }
}

#[derive(Debug, Clone, Copy, Eq, PartialEq)]
enum MidiFormat {
    Type0,
    Type1(u16),
    Type2(u16),
}

impl TryFrom<&[u8]> for MidiFormat {
    type Error = MidiConvertError;

    fn try_from(value: &[u8]) -> Result<Self, Self::Error> {
        if value.len() != 4 {
            return Err(MidiConvertError {
                msg: Some(format!("Midi format must be 4 bytes, got {}", value.len())),
                source: MidiConvertErrorTypes::MidiFormat,
                #[cfg(HAS_BACKTRACE)]
                backtrace: Backtrace::capture(),
            });
        }

        let ntracks = u16::from_be_bytes(value[2..4].try_into().unwrap());
        match u16::from_be_bytes(value[0..2].try_into().unwrap()) {
            0 => {
                if ntracks != 1 {
                    return Err(MidiConvertError {
                        msg: Some(format!(
                            "A midi format of 0 must use exactly 1 track, got {}",
                            ntracks
                        )),
                        source: MidiConvertErrorTypes::MidiFormat,
                        #[cfg(HAS_BACKTRACE)]
                        backtrace: Backtrace::capture(),
                    });
                }
                Ok(Self::Type0)
            }
            1 => Ok(Self::Type1(ntracks)),
            2 => Ok(Self::Type2(ntracks)),
            x => Err(MidiConvertError {
                msg: Some(format!("Midi format must be 0, 1, or 2, got {}", x)),
                source: MidiConvertErrorTypes::MidiFormat,
                #[cfg(HAS_BACKTRACE)]
                backtrace: Backtrace::capture(),
            }),
        }
    }
}

impl From<MidiFormat> for [u8; 4] {
    fn from(format: MidiFormat) -> Self {
        match format {
            MidiFormat::Type0 => [0, 0, 0, 1],
            MidiFormat::Type1(ntracks) => ((1_u32 << 16) + ntracks as u32).to_be_bytes(),
            MidiFormat::Type2(ntracks) => ((2_u32 << 16) + ntracks as u32).to_be_bytes(),
        }
    }
}

#[repr(u8)]
#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord)]
enum SmpteFps {
    Fmt24 = 24,
    Fmt25 = 25,
    Fmt2997 = 29,
    Fmt30 = 30,
}

impl TryFrom<u8> for SmpteFps {
    type Error = MidiConvertError;
    fn try_from(value: u8) -> Result<Self, Self::Error> {
        match value {
            24 => Ok(Self::Fmt24),
            25 => Ok(Self::Fmt25),
            29 => Ok(Self::Fmt2997),
            30 => Ok(Self::Fmt30),
            _ => Err(MidiConvertError {
                msg: Some("Midi format must be one of 24, 25, 29, or 30".into()),
                source: MidiConvertErrorTypes::Smpte,
                #[cfg(HAS_BACKTRACE)]
                backtrace: Backtrace::capture(),
            }),
        }
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
enum TimeDivision {
    TicksPerBeat(u16),
    FramesPerSecond(SmpteFps, u8),
}

impl TryFrom<&[u8]> for TimeDivision {
    type Error = MidiConvertError;

    fn try_from(bytes: &[u8]) -> Result<Self, Self::Error> {
        if bytes.len() != 2 {
            return Err(MidiConvertError {
                msg: Some(format!(
                    "Must be exactly 2 bytes to convert to TimeDivision, got {}",
                    bytes.len()
                )),
                source: MidiConvertErrorTypes::TimeDivision,
                #[cfg(HAS_BACKTRACE)]
                backtrace: Backtrace::capture(),
            });
        }
        if bytes[0] & BYTE_HIGH_BIT == BYTE_HIGH_BIT {
            Ok(TimeDivision::FramesPerSecond(
                (bytes[0] - BYTE_HIGH_BIT).try_into()?,
                bytes[1],
            ))
        } else {
            Ok(TimeDivision::TicksPerBeat(u16::from_be_bytes(
                bytes.try_into().unwrap(),
            )))
        }
    }
}

impl From<TimeDivision> for [u8; 2] {
    fn from(time_div: TimeDivision) -> Self {
        match time_div {
            TimeDivision::TicksPerBeat(ticks) => ticks.to_be_bytes(),
            TimeDivision::FramesPerSecond(fps, tpf) => {
                ((fps as u16 + BYTE_HIGH_BIT as u16) << 8 | tpf as u16).to_be_bytes()
            }
        }
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
struct HeaderChunk {
    format_type: MidiFormat,
    time_division: TimeDivision,
}

impl Default for HeaderChunk {
    fn default() -> Self {
        Self {
            format_type: MidiFormat::Type0,
            time_division: TimeDivision::TicksPerBeat(480),
        }
    }
}

impl From<HeaderChunk> for Vec<u8> {
    fn from(header_chunk: HeaderChunk) -> Self {
        let mut chunk = Vec::with_capacity(14);
        chunk.extend_from_slice(HEADER_CHUNK_ID); // 4
        chunk.extend_from_slice(6_u32.to_be_bytes().as_slice()); // 4
        chunk.extend_from_slice(<[u8; 4]>::from(header_chunk.format_type).as_ref()); // 4
        let time_division: [u8; 2] = header_chunk.time_division.into();
        chunk.extend_from_slice(&time_division);
        chunk
    }
}

impl TryFrom<&[u8]> for HeaderChunk {
    type Error = MidiConvertError;

    fn try_from(value: &[u8]) -> Result<Self, Self::Error> {
        if value.len() != 14 {
            return Err(MidiConvertError {
                msg: Some(format!(
                    "Header wrong size, given a slice {} bytes long, must be 14 bytes",
                    value.len()
                )),
                source: MidiConvertErrorTypes::HeaderChunk,
                #[cfg(HAS_BACKTRACE)]
                backtrace: Backtrace::capture(),
            });
        }
        let _chunk_id: ChunkId = value[0..4].try_into()?;
        let chunk_len = u32::from_be_bytes(value[4..8].try_into().unwrap());
        if chunk_len != 6 {
            return Err(MidiConvertError {
                msg: Some(format!(
                    "Chunk length must be 6 for a header chunk, got {}",
                    chunk_len
                )),
                source: MidiConvertErrorTypes::HeaderChunk,
                #[cfg(HAS_BACKTRACE)]
                backtrace: Backtrace::capture(),
            });
        }
        let format_type = value[8..12].try_into()?;
        let time_division = value[12..14].try_into()?;

        Ok(Self {
            format_type,
            time_division,
        })
    }
}

#[derive(Debug, PartialEq, Eq)]
struct Midi {
    header: HeaderChunk,
    tracks: Vec<TrackChunk>,
}

impl TryFrom<&[u8]> for Midi {
    type Error = MidiConvertError;

    fn try_from(bytes: &[u8]) -> Result<Self, Self::Error> {
        let header = <HeaderChunk>::try_from(&bytes[..14])?;
        let num_tracks = match header.format_type {
            MidiFormat::Type0 => 1,
            MidiFormat::Type1(num) => num,
            MidiFormat::Type2(num) => num,
        };
        let mut tracks = Vec::with_capacity(num_tracks as usize);
        let mut offset = 14;
        for _ in 0..num_tracks {
            let track = <TrackChunk>::try_from(&bytes[offset..])?;
            offset += track.chunk_size as usize + 8;
            tracks.push(track);
        }

        Ok(Self { header, tracks })
    }
}

fn read_variable(bytes: &[u8]) -> Result<(u32, usize), MidiConvertError> {
    let mut num: u32 = 0;
    let mut i = 0;
    for _ in 0..4 {
        if bytes[i..].is_empty() {
            return Err(MidiConvertError {
                msg: Some(
                    "Bytes was not long enough when trying to read variable length value!".into(),
                ),
                source: MidiConvertErrorTypes::VariableLength,
                #[cfg(HAS_BACKTRACE)]
                backtrace: Backtrace::capture(),
            });
        }
        num = (num << 7) | ((bytes[i] & BYTE_LOW_BITS) as u32);
        if bytes[i] & BYTE_HIGH_BIT == 0 {
            break;
        } else if i == 3 {
            return Err(MidiConvertError {
                msg: Some("Variable length values are a maximum of 4 bytes".into()),
                source: MidiConvertErrorTypes::VariableLength,
                #[cfg(HAS_BACKTRACE)]
                backtrace: Backtrace::capture(),
            });
        }
        i += 1;
    }
    Ok((num, i + 1))
}

fn write_variable(num: u32) -> Result<(Vec<u8>, usize), MidiConvertError> {
    if num > 0x0FFFFFFF {
        return Err(MidiConvertError {
            msg: Some("Number is too big to be stored in variable length!".into()),
            source: MidiConvertErrorTypes::VariableLength,
            #[cfg(HAS_BACKTRACE)]
            backtrace: Backtrace::capture(),
        });
    }
    let mut out_bytes = Vec::from([num as u8 & BYTE_LOW_BITS]);
    let mut num = num >> 7;
    let mut i = 1;
    while num > 0 {
        out_bytes.insert(0, BYTE_HIGH_BIT | num as u8);
        num >>= 7;
        i += 1;
    }
    Ok((out_bytes, i))
}

#[derive(Debug, PartialEq, Eq)]
struct TrackChunk {
    chunk_size: u32,
    midi_events: HashMap<u32, Vec<TrackEvent>>,
}

impl TryFrom<&[u8]> for TrackChunk {
    type Error = MidiConvertError;

    fn try_from(bytes: &[u8]) -> Result<Self, Self::Error> {
        if <ChunkId>::try_from(&bytes[0..4])? != ChunkId::Track {
            return Err(MidiConvertError {
                msg: Some("Not in a track chunk!".into()),
                source: MidiConvertErrorTypes::EventParse,
                #[cfg(HAS_BACKTRACE)]
                backtrace: Backtrace::capture(),
            });
        }
        let chunk_size = u32::from_be_bytes(bytes[4..8].try_into().unwrap());
        let mut size = chunk_size;
        let mut bytes = &bytes[8..];
        let mut status_byte: Option<u8> = None;
        let mut events: HashMap<u32, Vec<TrackEvent>> = HashMap::new();
        while size > 0 {
            let event = <TrackEvent>::try_from(StatusAndBytes(status_byte, bytes))?;
            if let Some(status) = event.status {
                status_byte = Some(status);
            }
            if event.event == TrackEventType::Meta(MetaEvent::EndOfTrack) {
                events
                    .entry(event.delta_time)
                    .and_modify(|v| v.push(event))
                    .or_insert(Vec::new());
                break;
            }
            events
                .entry(event.delta_time)
                .and_modify(|v| v.push(event.clone()))
                .or_insert_with(|| vec![event.clone()]);
            size -= event.size as u32;
            bytes = &bytes[event.size..];
        }
        Ok(Self {
            chunk_size,
            midi_events: events,
        })
    }
}

#[derive(Debug, PartialEq, Eq, Clone)]
struct TrackEvent {
    delta_time: u32,
    event: TrackEventType,
    size: usize,
    status: Option<u8>,
}

#[derive(Debug, PartialEq, Eq)]
struct StatusAndBytes<'a>(Option<u8>, &'a [u8]);

impl<'a> TryFrom<StatusAndBytes<'a>> for TrackEvent {
    type Error = MidiConvertError;

    fn try_from(data: StatusAndBytes<'a>) -> Result<Self, Self::Error> {
        let (delta_time, mut size) = read_variable(data.1)?;
        let needed_bytes = &data.1[size..];
        let mut status = data.0;
        let first_byte = needed_bytes.first().unwrap();
        let event = match (data.0, first_byte) {
            (_, 0x80..=0xEF) => {
                status = Some(*first_byte);
                let midi_event = <MidiEvent>::try_from(StatusAndEvent(None, needed_bytes))?;
                size += midi_event.size;
                TrackEventType::Midi(midi_event)
            }
            (_, 0xF0..=0xF7) => {
                status = None;
                let sysex_event = <SysexEventSized>::try_from((needed_bytes, false))?;
                size += sysex_event.0;
                TrackEventType::Sysex(sysex_event.1)
            }
            (_, 0xFF) => {
                status = None;
                let meta_event = <MetaEventSized>::try_from(&needed_bytes[1..])?;
                size += meta_event.0 + 1;
                TrackEventType::Meta(meta_event.1)
            }
            (_, 0xF8..=0xFE) => {
                return Err(MidiConvertError {
                    msg: Some("Not a valid status byte!".into()),
                    source: MidiConvertErrorTypes::EventParse,
                    #[cfg(HAS_BACKTRACE)]
                    backtrace: Backtrace::capture(),
                });
            }
            (Some(_), _) => {
                let midi_event = <MidiEvent>::try_from(StatusAndEvent(status, needed_bytes))?;
                size += midi_event.size;
                TrackEventType::Midi(midi_event)
            }
            (_, _) => {
                return Err(MidiConvertError {
                    msg: Some("Not a valid status byte!".into()),
                    source: MidiConvertErrorTypes::EventParse,
                    #[cfg(HAS_BACKTRACE)]
                    backtrace: Backtrace::capture(),
                });
            }
        };

        Ok(Self {
            delta_time,
            event,
            size,
            status,
        })
    }
}

impl From<TrackEvent> for Vec<u8> {
    fn from(tte: TrackEvent) -> Self {
        let mut out = Vec::new();
        out.extend(write_variable(tte.delta_time).unwrap().0);
        match tte.event {
            TrackEventType::Midi(me) => out.extend(<Vec<u8>>::from(me)),
            TrackEventType::Sysex(se) => out.extend(<Vec<u8>>::from(se)),
            TrackEventType::Meta(me) => out.extend(<Vec<u8>>::from(me)),
        }

        out
    }
}

#[derive(Debug, PartialEq, Eq, Clone)]
enum TrackEventType {
    Midi(MidiEvent),
    Sysex(SysexEvent),
    Meta(MetaEvent),
}

#[derive(Debug, Copy, Clone, PartialEq, Eq)]
struct MidiEvent {
    channel: Channel,
    event_type: MidiEventType,
    size: usize,
}

#[derive(Debug, Copy, Clone, PartialEq, Eq)]
struct StatusAndEvent<'a>(Option<u8>, &'a [u8]);

impl<'a> TryFrom<StatusAndEvent<'a>> for MidiEvent {
    type Error = MidiConvertError;

    fn try_from(data: StatusAndEvent<'a>) -> Result<Self, Self::Error> {
        let err1 = MidiConvertError {
            msg: Some("Bytes does not have enough data bytes!".into()),
            source: MidiConvertErrorTypes::EventParse,
            #[cfg(HAS_BACKTRACE)]
            backtrace: Backtrace::capture(),
        };

        let (byte, plus) = if let Some(byte) = data.0 {
            (byte, 0)
        } else {
            (*data.1.first().unwrap(), 1)
        };
        match byte {
            0x80..=0x8F => Ok(Self {
                size: 2 + plus,
                channel: byte.into(),
                event_type: MidiEventType::NoteOff(data.1[plus], data.1[1 + plus]),
            }),
            0x90..=0x9F => Ok(Self {
                size: 2 + plus,
                channel: byte.into(),
                event_type: MidiEventType::NoteOn(data.1[plus], data.1[1 + plus]),
            }),
            0xA0..=0xAF => Ok(Self {
                size: 2 + plus,
                channel: byte.into(),
                event_type: MidiEventType::NoteAftertouch(data.1[plus], data.1[1 + plus]),
            }),
            0xB0..=0xBF => Ok(Self {
                size: 2 + plus,
                channel: byte.into(),
                event_type: MidiEventType::Controller(data.1[plus], data.1[1 + plus]),
            }),
            0xC0..=0xCF => Ok(Self {
                size: 1 + plus,
                channel: byte.into(),
                event_type: MidiEventType::ProgramChange(data.1[plus]),
            }),
            0xD0..=0xDF => Ok(Self {
                size: 1 + plus,
                channel: byte.into(),
                event_type: MidiEventType::ChannelAftertouch(data.1[plus]),
            }),
            0xE0..=0xEF => Ok(Self {
                size: 2 + plus,
                channel: byte.into(),
                event_type: MidiEventType::PitchBend(u16::from_be_bytes([
                    data.1[plus],
                    data.1[plus + 1],
                ])),
            }),
            _ => Err(err1),
        }
    }
}

impl From<MidiEvent> for Vec<u8> {
    fn from(event: MidiEvent) -> Self {
        match event.event_type {
            MidiEventType::NoteOff(note, vel) => vec![0x8 << 4 | event.channel as u8, note, vel],
            MidiEventType::NoteOn(note, vel) => vec![0x9 << 4 | event.channel as u8, note, vel],
            MidiEventType::NoteAftertouch(note, aft) => {
                vec![0xA << 4 | event.channel as u8, note, aft]
            }
            MidiEventType::Controller(num, val) => vec![0xB << 4 | event.channel as u8, num, val],
            MidiEventType::ProgramChange(prog) => vec![0xC << 4 | event.channel as u8, prog],
            MidiEventType::ChannelAftertouch(aft) => vec![0xD << 4 | event.channel as u8, aft],
            MidiEventType::PitchBend(val) => Vec::from(val.to_be_bytes()),
        }
    }
}

#[repr(u8)]
#[derive(Debug, Copy, Clone, PartialEq, Eq)]
enum Channel {
    Ch0 = 0,
    Ch1,
    Ch2,
    Ch3,
    Ch4,
    Ch5,
    Ch6,
    Ch7,
    Ch8,
    Ch9,
    ChA,
    ChB,
    ChC,
    ChD,
    ChE,
    ChF,
}

impl From<u8> for Channel {
    fn from(byte: u8) -> Self {
        let byte = byte & 0x0F;
        match byte {
            0x0 => Channel::Ch0,
            0x1 => Channel::Ch1,
            0x2 => Channel::Ch2,
            0x3 => Channel::Ch3,
            0x4 => Channel::Ch4,
            0x5 => Channel::Ch5,
            0x6 => Channel::Ch6,
            0x7 => Channel::Ch7,
            0x8 => Channel::Ch8,
            0x9 => Channel::Ch9,
            0xA => Channel::ChA,
            0xB => Channel::ChB,
            0xC => Channel::ChC,
            0xD => Channel::ChD,
            0xE => Channel::ChE,
            _ => Channel::ChF,
        }
    }
}

#[derive(Debug, Copy, Clone, PartialEq, Eq)]
enum MidiEventType {
    NoteOff(u8, u8),
    NoteOn(u8, u8),
    NoteAftertouch(u8, u8),
    Controller(u8, u8),
    ProgramChange(u8),
    ChannelAftertouch(u8),
    PitchBend(u16),
}

#[derive(Debug, PartialEq, Eq)]
struct SysexEventSized(usize, SysexEvent);

#[derive(Debug, PartialEq, Eq, Clone)]
enum SysexEvent {
    Normal(Vec<u8>),
    BeginDivided(Vec<u8>),
    MiddleDivided(Vec<u8>),
    EndDivided(Vec<u8>),
    Authorization(Vec<u8>),
}

impl TryFrom<(&[u8], bool)> for SysexEventSized {
    type Error = MidiConvertError;

    fn try_from(data: (&[u8], bool)) -> Result<Self, Self::Error> {
        let (length, length_len) = read_variable(&data.0[1..])?;
        let needed_bytes = &data.0[length_len + 1..length as usize + length_len + 1];
        let total = length as usize + length_len + 1;
        match data.0.first().unwrap() {
            0xF0 => {
                if needed_bytes[length as usize - 1] == 0xF7 {
                    Ok(Self(
                        total,
                        SysexEvent::Normal(needed_bytes[..length as usize - 1].to_vec()),
                    ))
                } else {
                    Ok(Self(total, SysexEvent::BeginDivided(needed_bytes.to_vec())))
                }
            }
            0xF7 => {
                if data.1 {
                    if needed_bytes[length as usize - 1] == 0xF7 {
                        Ok(Self(total, SysexEvent::EndDivided(needed_bytes.to_vec())))
                    } else {
                        Ok(Self(
                            total,
                            SysexEvent::MiddleDivided(needed_bytes[..length as usize - 1].to_vec()),
                        ))
                    }
                } else {
                    Ok(Self(
                        total,
                        SysexEvent::Authorization(needed_bytes.to_vec()),
                    ))
                }
            }
            _ => Err(MidiConvertError {
                msg: Some("wrong byte for sysex!".into()),
                source: MidiConvertErrorTypes::EventParse,
                #[cfg(HAS_BACKTRACE)]
                backtrace: Backtrace::capture(),
            }),
        }
    }
}

impl From<SysexEvent> for Vec<u8> {
    fn from(sxe: SysexEvent) -> Self {
        let mut out = Vec::new();
        match sxe {
            SysexEvent::Normal(data) => {
                out.push(0xF0);
                out.extend(write_variable(data.len() as u32).unwrap().0);
                out.extend(data);
                out.push(0xF7);
            }
            SysexEvent::BeginDivided(data) => {
                out.push(0xF0);
                out.extend(write_variable(data.len() as u32).unwrap().0);
                out.extend(data);
            }
            SysexEvent::MiddleDivided(data) | SysexEvent::Authorization(data) => {
                out.push(0xF7);
                out.extend(write_variable(data.len() as u32).unwrap().0);
                out.extend(data);
            }
            SysexEvent::EndDivided(data) => {
                out.push(0xF7);
                out.extend(write_variable(data.len() as u32).unwrap().0);
                out.extend(data);
                out.push(0xF7);
            }
        }
        out
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
enum KeySignatureAccidentals {
    C,
    G,
    D,
    A,
    E,
    B,
    CFlat,
    FSharp,
    GFlat,
    CSharp,
    DFlat,
    AFlat,
    EFlat,
    BFlat,
    F,
}

impl TryFrom<i8> for KeySignatureAccidentals {
    type Error = MidiConvertError;

    fn try_from(value: i8) -> Result<Self, Self::Error> {
        match value {
            -7 => Ok(Self::CFlat),
            -6 => Ok(Self::GFlat),
            -5 => Ok(Self::DFlat),
            -4 => Ok(Self::AFlat),
            -3 => Ok(Self::EFlat),
            -2 => Ok(Self::BFlat),
            -1 => Ok(Self::F),
            0 => Ok(Self::C),
            1 => Ok(Self::G),
            2 => Ok(Self::D),
            3 => Ok(Self::A),
            4 => Ok(Self::E),
            5 => Ok(Self::B),
            6 => Ok(Self::FSharp),
            7 => Ok(Self::CSharp),
            _ => Err(MidiConvertError {
                msg: Some("Can't convert num to key signature!".into()),
                source: MidiConvertErrorTypes::EventParse,
                #[cfg(HAS_BACKTRACE)]
                backtrace: Backtrace::capture(),
            }),
        }
    }
}

impl From<KeySignatureAccidentals> for i8 {
    fn from(key: KeySignatureAccidentals) -> Self {
        match key {
            KeySignatureAccidentals::C => 0,
            KeySignatureAccidentals::G => 1,
            KeySignatureAccidentals::D => 2,
            KeySignatureAccidentals::A => 3,
            KeySignatureAccidentals::E => 4,
            KeySignatureAccidentals::B => 5,
            KeySignatureAccidentals::CFlat => -7,
            KeySignatureAccidentals::FSharp => 6,
            KeySignatureAccidentals::GFlat => -6,
            KeySignatureAccidentals::CSharp => 7,
            KeySignatureAccidentals::DFlat => -5,
            KeySignatureAccidentals::AFlat => -4,
            KeySignatureAccidentals::EFlat => -3,
            KeySignatureAccidentals::BFlat => -2,
            KeySignatureAccidentals::F => -1,
        }
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
#[repr(u8)]
enum KeyQuality {
    Major = 0,
    Minor,
}

impl TryFrom<u8> for KeyQuality {
    type Error = MidiConvertError;

    fn try_from(value: u8) -> Result<Self, Self::Error> {
        match value {
            0 => Ok(Self::Major),
            1 => Ok(Self::Minor),
            _ => Err(MidiConvertError {
                msg: Some("Can't convert num to key signature!".into()),
                source: MidiConvertErrorTypes::EventParse,
                #[cfg(HAS_BACKTRACE)]
                backtrace: Backtrace::capture(),
            }),
        }
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
struct KeySignature {
    accidentals: KeySignatureAccidentals,
    quality: KeyQuality,
}

impl TryFrom<&[u8]> for KeySignature {
    type Error = MidiConvertError;

    fn try_from(value: &[u8]) -> Result<Self, Self::Error> {
        if value.len() < 2 {
            return Err(MidiConvertError {
                msg: Some("Need more info!".into()),
                source: MidiConvertErrorTypes::EventParse,
                #[cfg(HAS_BACKTRACE)]
                backtrace: Backtrace::capture(),
            });
        }
        Ok(Self {
            accidentals: (value[0] as i8).try_into()?,
            quality: value[1].try_into()?,
        })
    }
}

impl From<KeySignature> for [u8; 2] {
    fn from(ks: KeySignature) -> Self {
        [<i8>::from(ks.accidentals) as u8, ks.quality as u8]
    }
}

#[derive(Debug, PartialEq, Eq)]
struct MetaEventSized(usize, MetaEvent);

#[derive(Debug, PartialEq, Eq, Clone)]
enum MetaEvent {
    SequenceNumber(u16),
    TextEvent(String),
    CopyrightNotice(String),
    SequenceOrTrackName(String),
    InstrumentName(String),
    Lyrics(String),
    Marker(String),
    CuePoint(String),
    MidiChannelPrefix(Channel),
    EndOfTrack,
    SetTempo(u32),
    SmpteOffset(u8, u8, u8, u8, u8),
    TimeSignature(u8, u8, u8, u8),
    KeySignature(KeySignature),
    SequencerSpecific(Vec<u8>),
}

impl TryFrom<&[u8]> for MetaEventSized {
    type Error = MidiConvertError;

    fn try_from(bytes: &[u8]) -> Result<Self, Self::Error> {
        let (length, length_len) = read_variable(&bytes[1..])?;
        let needed_bytes = &bytes[length_len + 1..length as usize + length_len + 1];
        let total = length as usize + length_len + 1;
        match bytes.first().ok_or(MidiConvertError {
            msg: Some("Not enough bytes in bytes.".into()),
            source: MidiConvertErrorTypes::EventParse,
            #[cfg(HAS_BACKTRACE)]
            backtrace: Backtrace::capture(),
        })? {
            0x00 => Ok(Self(
                total,
                MetaEvent::SequenceNumber(u16::from_be_bytes(needed_bytes.try_into().unwrap())),
            )),
            0x01 => Ok(Self(
                total,
                MetaEvent::TextEvent(String::from_utf8(needed_bytes.to_vec()).unwrap()),
            )),
            0x02 => Ok(Self(
                total,
                MetaEvent::CopyrightNotice(String::from_utf8(needed_bytes.to_vec()).unwrap()),
            )),
            0x03 => Ok(Self(
                total,
                MetaEvent::SequenceOrTrackName(String::from_utf8(needed_bytes.to_vec()).unwrap()),
            )),
            0x04 => Ok(Self(
                total,
                MetaEvent::InstrumentName(String::from_utf8(needed_bytes.to_vec()).unwrap()),
            )),
            0x05 => Ok(Self(
                total,
                MetaEvent::Lyrics(String::from_utf8(needed_bytes.to_vec()).unwrap()),
            )),
            0x06 => Ok(Self(
                total,
                MetaEvent::Marker(String::from_utf8(needed_bytes.to_vec()).unwrap()),
            )),
            0x07 => Ok(Self(
                total,
                MetaEvent::CuePoint(String::from_utf8(needed_bytes.to_vec()).unwrap()),
            )),
            0x20 => Ok(Self(
                total,
                MetaEvent::MidiChannelPrefix(<Channel>::from(needed_bytes[0])),
            )),
            0x2F => Ok(Self(total, MetaEvent::EndOfTrack)),
            0x51 => Ok(Self(
                total,
                MetaEvent::SetTempo(u32::from_be_bytes([
                    0,
                    needed_bytes[0],
                    needed_bytes[1],
                    needed_bytes[2],
                ])),
            )),
            0x54 => {
                let max_framerate = match needed_bytes[0] & 0x60 {
                    0x00 => 24,
                    0x20 => 25,
                    _ => 30,
                };
                if needed_bytes[0] >= 0x80
                    || needed_bytes[0] & 0x1F >= 24
                    || needed_bytes[1] >= 60
                    || needed_bytes[2] >= 60
                    || needed_bytes[3] >= max_framerate
                    || needed_bytes[4] >= 100
                {
                    Err(MidiConvertError {
                        msg: Some("Invalid Smpte Offset!".into()),
                        source: MidiConvertErrorTypes::EventParse,
                        #[cfg(HAS_BACKTRACE)]
                        backtrace: Backtrace::capture(),
                    })
                } else {
                    Ok(Self(
                        total,
                        MetaEvent::SmpteOffset(
                            needed_bytes[0],
                            needed_bytes[1],
                            needed_bytes[2],
                            needed_bytes[3],
                            needed_bytes[4],
                        ),
                    ))
                }
            }
            0x58 => Ok(Self(
                total,
                MetaEvent::TimeSignature(
                    needed_bytes[0],
                    needed_bytes[1],
                    needed_bytes[2],
                    needed_bytes[3],
                ),
            )),
            0x59 => Ok(Self(
                total,
                MetaEvent::KeySignature(needed_bytes.try_into()?),
            )),
            0x7F => Ok(Self(
                total,
                MetaEvent::SequencerSpecific(needed_bytes.to_vec()),
            )),
            x => Err(MidiConvertError {
                msg: Some(format!("Invalid error code: {}", x)),
                source: MidiConvertErrorTypes::EventParse,
                #[cfg(HAS_BACKTRACE)]
                backtrace: Backtrace::capture(),
            }),
        }
    }
}

impl From<MetaEvent> for Vec<u8> {
    fn from(me: MetaEvent) -> Self {
        let mut out = vec![0xFFu8];
        match me {
            MetaEvent::SequenceNumber(a) => {
                out.push(0x00);
                out.extend(write_variable(2).unwrap().0);
                out.push(a.to_be_bytes()[0]);
                out.push(a.to_be_bytes()[1]);
            }
            MetaEvent::TextEvent(msg) => {
                if !msg.is_ascii() {
                    panic!("Not ascii!");
                }
                out.push(0x01);
                out.extend(write_variable(msg.len() as u32).unwrap().0);
                out.extend_from_slice(msg.as_bytes());
            }
            MetaEvent::CopyrightNotice(msg) => {
                if !msg.is_ascii() {
                    panic!("Not ascii!");
                }
                out.push(0x02);
                out.extend(write_variable(msg.len() as u32).unwrap().0);
                out.extend_from_slice(msg.as_bytes());
            }
            MetaEvent::SequenceOrTrackName(msg) => {
                if !msg.is_ascii() {
                    panic!("Not ascii!");
                }
                out.push(0x03);
                out.extend(write_variable(msg.len() as u32).unwrap().0);
                out.extend_from_slice(msg.as_bytes());
            }
            MetaEvent::InstrumentName(msg) => {
                if !msg.is_ascii() {
                    panic!("Not ascii!");
                }
                out.push(0x04);
                out.extend(write_variable(msg.len() as u32).unwrap().0);
                out.extend_from_slice(msg.as_bytes());
            }
            MetaEvent::Lyrics(msg) => {
                if !msg.is_ascii() {
                    panic!("Not ascii!");
                }
                out.push(0x05);
                out.extend(write_variable(msg.len() as u32).unwrap().0);
                out.extend_from_slice(msg.as_bytes());
            }
            MetaEvent::Marker(msg) => {
                if !msg.is_ascii() {
                    panic!("Not ascii!");
                }
                out.push(0x06);
                out.extend(write_variable(msg.len() as u32).unwrap().0);
                out.extend_from_slice(msg.as_bytes());
            }
            MetaEvent::CuePoint(msg) => {
                if !msg.is_ascii() {
                    panic!("Not ascii!");
                }
                out.push(0x07);
                out.extend(write_variable(msg.len() as u32).unwrap().0);
                out.extend_from_slice(msg.as_bytes());
            }
            MetaEvent::MidiChannelPrefix(chan) => {
                out.push(0x20);
                out.extend(write_variable(1).unwrap().0);
                out.push(chan as u8);
            }
            MetaEvent::EndOfTrack => {
                out.push(0x2F);
                out.extend(write_variable(0).unwrap().0);
            }
            MetaEvent::SetTempo(tempo) => {
                out.push(0x51);
                out.extend(write_variable(3).unwrap().0);
                out.push(tempo.to_be_bytes()[0]);
                out.push(tempo.to_be_bytes()[1]);
                out.push(tempo.to_be_bytes()[2]);
            }
            MetaEvent::SmpteOffset(hr, mn, se, fr, ff) => {
                out.push(0x54);
                out.extend(write_variable(5).unwrap().0);
                out.push(hr);
                out.push(mn);
                out.push(se);
                out.push(fr);
                out.push(ff);
            }
            MetaEvent::TimeSignature(num, den, met, snds) => {
                out.push(0x58);
                out.extend(write_variable(4).unwrap().0);
                out.push(num);
                out.push(den);
                out.push(met);
                out.push(snds);
            }
            MetaEvent::KeySignature(key_sig) => {
                out.push(0x59);
                out.extend(write_variable(2).unwrap().0);
                out.extend_from_slice(&<[u8; 2]>::from(key_sig));
            }
            MetaEvent::SequencerSpecific(data) => {
                out.push(0x7F);
                out.extend(write_variable(data.len() as u32).unwrap().0);
                out.extend(data);
            }
        }
        out
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_chunk_id_try_from() -> Result<(), MidiConvertError> {
        let header_chunk_id: ChunkId = HEADER_CHUNK_ID.as_slice().try_into()?;
        assert_eq!(header_chunk_id, ChunkId::Header);
        let track_chunk_id: ChunkId = TRACK_CHUNK_ID.as_slice().try_into()?;
        assert_eq!(track_chunk_id, ChunkId::Track);
        let trid: Result<ChunkId, MidiConvertError> = b"Hell".as_slice().try_into();
        assert!(trid.is_err());
        assert_eq!(b"MThd", <&[u8]>::from(ChunkId::Header));
        assert_eq!(b"MTrk", <&[u8]>::from(ChunkId::Track));

        Ok(())
    }

    #[test]
    fn test_midi_format_convert() -> Result<(), MidiConvertError> {
        let mid_fmt0: MidiFormat = 0x00000001u32.to_be_bytes().as_slice().try_into()?;
        let mid_fmt1: MidiFormat = 0x00010024u32.to_be_bytes().as_slice().try_into()?;
        let mid_fmt2: MidiFormat = 0x00020045u32.to_be_bytes().as_slice().try_into()?;
        let mid_fmt3: Result<MidiFormat, MidiConvertError> =
            0x00030042u32.to_be_bytes().as_slice().try_into();
        assert_eq!(mid_fmt0, MidiFormat::Type0);
        assert_eq!(mid_fmt1, MidiFormat::Type1(36));
        assert_eq!(mid_fmt2, MidiFormat::Type2(69));
        assert!(mid_fmt3.is_err());
        let fmt_two = u32::from_be_bytes(<[u8; 4]>::from(MidiFormat::Type2(43)));
        assert_eq!(fmt_two, 0x0002002B);
        Ok(())
    }

    #[test]
    fn test_smpte_fps() -> Result<(), MidiConvertError> {
        let smpte1: SmpteFps = 24u8.try_into()?;
        let smpte2: SmpteFps = 25u8.try_into()?;
        let smpte3: SmpteFps = 29u8.try_into()?;
        let smpte4: SmpteFps = 30u8.try_into()?;
        let smpte_err: Result<SmpteFps, MidiConvertError> = 38u8.try_into();
        assert_eq!(smpte1, SmpteFps::Fmt24);
        assert_eq!(smpte2, SmpteFps::Fmt25);
        assert_eq!(smpte3, SmpteFps::Fmt2997);
        assert_eq!(smpte4, SmpteFps::Fmt30);
        assert!(smpte_err.is_err());
        let fmt_back = SmpteFps::Fmt2997 as u8;
        assert_eq!(fmt_back, 29u8);
        Ok(())
    }

    #[test]
    fn test_time_division() -> Result<(), MidiConvertError> {
        let time_div1: TimeDivision = 0x9840_u16.to_be_bytes().as_slice().try_into()?;
        let time_div2: TimeDivision = 0x9D29_u16.to_be_bytes().as_slice().try_into()?;
        let time_div3: TimeDivision = 0x0074_u16.to_be_bytes().as_slice().try_into()?;
        let time_div4_err: Result<TimeDivision, MidiConvertError> =
            0x8383_u16.to_be_bytes().as_slice().try_into();
        let time_div5_err: Result<TimeDivision, MidiConvertError> =
            0xF840_u16.to_be_bytes().as_slice().try_into();
        assert_eq!(
            time_div1,
            TimeDivision::FramesPerSecond(SmpteFps::Fmt24, 64)
        );
        assert_eq!(
            time_div2,
            TimeDivision::FramesPerSecond(SmpteFps::Fmt2997, 41)
        );
        assert_eq!(time_div3, TimeDivision::TicksPerBeat(0x74));
        assert!(time_div4_err.is_err());
        assert!(time_div5_err.is_err());

        let time_div_bytes: [u8; 2] = TimeDivision::TicksPerBeat(0x73).into();
        let time_div_bytes2: [u8; 2] = TimeDivision::FramesPerSecond(SmpteFps::Fmt25, 69).into();
        assert_eq!(time_div_bytes, 0x0073_u16.to_be_bytes());
        dbg!(time_div_bytes2);
        assert_eq!(time_div_bytes2, 0x9945_u16.to_be_bytes());
        Ok(())
    }

    #[test]
    fn test_header_chunk() -> Result<(), MidiConvertError> {
        let header_chunk_bytes: [u8; 14] = [
            0x4D, 0x54, 0x68, 0x64, 0x00, 0x00, 0x00, 0x06, 0x00, 0x00, 0x00, 0x01, 0x01, 0xE0,
        ];
        let header_chunk: HeaderChunk = header_chunk_bytes.as_slice().try_into()?;
        let header_chunk2_bytes: [u8; 14] = [
            0x4D, 0x54, 0x68, 0x64, 0x00, 0x00, 0x00, 0x06, 0x00, 0x02, 0x00, 0x03, 0x99, 0x45,
        ];
        let header_chunk2: HeaderChunk = header_chunk2_bytes.as_slice().try_into()?;
        let bad_header: Result<HeaderChunk, MidiConvertError> = [
            0x4F, 0x54, 0x68, 0x64, 0x00, 0x00, 0x00, 0x06, 0x00, 0x00, 0x00, 0x01, 0x01, 0xE0,
        ]
        .as_slice()
        .try_into();
        let bad_chunk_len: Result<HeaderChunk, MidiConvertError> = [
            0x4D, 0x54, 0x68, 0x64, 0x00, 0x00, 0x30, 0x06, 0x00, 0x00, 0x00, 0x01, 0x01, 0xE0,
        ]
        .as_slice()
        .try_into();
        let bad_midi_version: Result<HeaderChunk, MidiConvertError> = [
            0x4D, 0x54, 0x68, 0x64, 0x00, 0x00, 0x00, 0x06, 0x03, 0x00, 0x00, 0x01, 0x01, 0xE0,
        ]
        .as_slice()
        .try_into();

        let cmp_header = HeaderChunk {
            format_type: MidiFormat::Type2(3),
            time_division: TimeDivision::FramesPerSecond(SmpteFps::Fmt25, 69),
        };

        assert_eq!(
            header_chunk_bytes.to_vec(),
            <Vec<u8>>::from(HeaderChunk::default())
        );
        assert_eq!(header_chunk2_bytes.to_vec(), <Vec<u8>>::from(cmp_header));

        assert_eq!(header_chunk, HeaderChunk::default());
        assert_eq!(header_chunk2, cmp_header,);
        assert!(bad_header.is_err());
        assert!(bad_chunk_len.is_err());
        assert!(bad_midi_version.is_err());

        Ok(())
    }

    #[test]
    fn test_read_variable() -> Result<(), MidiConvertError> {
        let variable: [u8; 1] = [0x00];
        let variable1: [u8; 1] = [0x40];
        let variable2: [u8; 1] = [0x7F];
        let variable3: [u8; 2] = [0x81, 0x00];
        let variable4: [u8; 2] = [0xC0, 0x00];
        let variable5: [u8; 2] = [0xFF, 0x7F];
        let variable6: [u8; 3] = [0x81, 0x80, 0x00];
        let variable7: [u8; 3] = [0xC0, 0x80, 0x00];
        let variable8: [u8; 3] = [0xFF, 0xFF, 0x7F];
        let variable9: [u8; 4] = [0x81, 0x80, 0x80, 0x00];
        let variable10: [u8; 4] = [0xC0, 0x80, 0x80, 0x00];
        let variable11: [u8; 4] = [0xFF, 0xFF, 0xFF, 0x7F];
        let variable12: [u8; 5] = [0xFF, 0xFF, 0xFF, 0xFF, 0x7F];
        let variable13: [u8; 0] = [];

        let var: u32 = 0x00;
        let var1: u32 = 0x40;
        let var2: u32 = 0x7F;
        let var3: u32 = 0x80;
        let var4: u32 = 0x2000;
        let var5: u32 = 0x3FFF;
        let var6: u32 = 0x4000;
        let var7: u32 = 0x100000;
        let var8: u32 = 0x1FFFFF;
        let var9: u32 = 0x200000;
        let var10: u32 = 0x08000000;
        let var11: u32 = 0x0FFFFFFF;
        let var12: u32 = 0xAFFFFFFF;

        assert_eq!(read_variable(variable.as_slice())?.0, var);
        assert_eq!(read_variable(variable1.as_slice())?.0, var1);
        assert_eq!(read_variable(variable2.as_slice())?.0, var2);
        assert_eq!(read_variable(variable3.as_slice())?.0, var3);
        assert_eq!(read_variable(variable4.as_slice())?.0, var4);
        assert_eq!(read_variable(variable5.as_slice())?.0, var5);
        assert_eq!(read_variable(variable6.as_slice())?.0, var6);
        assert_eq!(read_variable(variable7.as_slice())?.0, var7);
        assert_eq!(read_variable(variable8.as_slice())?.0, var8);
        assert_eq!(read_variable(variable9.as_slice())?.0, var9);
        assert_eq!(read_variable(variable10.as_slice())?.0, var10);
        assert_eq!(read_variable(variable11.as_slice())?.0, var11);
        assert_eq!(write_variable(var)?.0, variable.as_slice());
        assert_eq!(write_variable(var1)?.0, variable1.as_slice());
        assert_eq!(write_variable(var2)?.0, variable2.as_slice());
        assert_eq!(write_variable(var3)?.0, variable3.as_slice());
        assert_eq!(write_variable(var4)?.0, variable4.as_slice());
        assert_eq!(write_variable(var5)?.0, variable5.as_slice());
        assert_eq!(write_variable(var6)?.0, variable6.as_slice());
        assert_eq!(write_variable(var7)?.0, variable7.as_slice());
        assert_eq!(write_variable(var8)?.0, variable8.as_slice());
        assert_eq!(write_variable(var9)?.0, variable9.as_slice());
        assert_eq!(write_variable(var10)?.0, variable10.as_slice());
        assert_eq!(write_variable(var11)?.0, variable11.as_slice());
        assert!(read_variable(variable12.as_slice()).is_err());
        assert!(read_variable(variable13.as_slice()).is_err());
        assert!(write_variable(var12).is_err());

        Ok(())
    }

    // #[test]
    // fn test_midi_event_convert() -> Result<(), MidiConvertError> {
    //     let test0 = [0x80u8, 0x30u8, 0x40u8];
    //     let test1 = [0x93u8, 0x80u8, 0x38u8];

    //     assert_eq!(
    //         <MidiEvent>::try_from(StatusAndEvent(None, test0.as_slice()))?,
    //         MidiEventTwo(
    //             3,
    //             MidiEvent {
    //                 channel: Channel::Ch0,
    //                 event_type: MidiEventType::NoteOff(0x30, 0x40)
    //             }
    //         )
    //     );
    //     assert_eq!(
    //         <MidiEventSized>::try_from(test1.as_slice())?,
    //         MidiEventSized(
    //             3,
    //             MidiEvent {
    //                 channel: Channel::Ch3,
    //                 event_type: MidiEventType::NoteOn(0x80, 0x38),
    //             }
    //         )
    //     );
    //     assert_eq!(
    //         test0.to_vec(),
    //         <Vec<u8>>::from(MidiEvent {
    //             channel: Channel::Ch0,
    //             event_type: MidiEventType::NoteOff(0x30, 0x40)
    //         })
    //     );

    //     Ok(())
    // }

    #[test]
    fn test_parse_midi() -> Result<(), MidiConvertError> {
        use std::{fs::File, io::Read};

        let mut leyenda = File::open("leyenda.mid").unwrap();
        let mut bytes: Vec<u8> = Vec::new();
        leyenda.read_to_end(&mut bytes).unwrap();
        println!("{:?}", bytes);

        let midi: Midi = <Midi>::try_from(bytes.as_slice())?;
        for track in midi.tracks.iter() {
            let mut keys: Vec<u32> = Vec::new();
            for key in track.midi_events.keys() {
                keys.push(*key);
            }
            keys.sort_unstable();
            for key in keys {
                println!(
                    "Delta time: {}\nEvent: {:?}\n\n",
                    key,
                    track.midi_events.get(&key).unwrap()
                );
            }
        }

        // println!("{:?}", midi);

        Ok(())
    }
}
